import sys
import subprocess
import os
from os.path import basename
from PySide import QtCore, QtGui
from policedb import Ui_PoliceDB
from imageaddpage import Ui_WizardPage
import json
import thread

QIcon = QtGui.QIcon
QListWidgetItem = QtGui.QListWidgetItem
QListView = QtGui.QListView
QMessageBox = QtGui.QMessageBox
QItemSelection = QtGui.QItemSelection
QFileDialog = QtGui.QFileDialog
QPixmap = QtGui.QPixmap
QWizard = QtGui.QWizard
QSize = QtCore.QSize
QThread = QtCore.QThread
Signal = QtCore.Signal
Qt = QtCore.Qt


pathKey = 'path'
nameKey = 'name'
ageKey  = 'age'
descKey = 'desc'



class ControlImageAddPage(QtGui.QWizardPage):
  def __init__(self, imagePath, parent=None):
    super(ControlImageAddPage, self).__init__(parent)
    self.ui = Ui_WizardPage()
    self.ui.setupUi(self)
    self.loadImage(imagePath)


  # Skip using '.ui' all the time!
  def __getattr__(self, name):
    return self.ui.__getattribute__(name)


  def loadImage(self, imagePath):
    self.imagePath = imagePath
    self.filePath.setText(imagePath)
    self.imageLabel.setPixmap(QPixmap(imagePath))


#  Runs the policeUI gui
class ControlPoliceDB(QtGui.QMainWindow):

  # Size of icons in image list
  ICON_SIZE = QSize(200, 200)

  # Max size of 'selected suspect' image
  FULL_IMAGE_SIZE = QSize(400, 400)


  def __init__(self, parent=None):
    super(ControlPoliceDB, self).__init__(parent)
    self.ui =  Ui_PoliceDB()
    self.ui.setupUi(self)


    self.imageList.setIconSize(ControlPoliceDB.ICON_SIZE)

    # Selection handler
    self.imageList.selectionChanged = self.gridImageSelected

    # Button handlers
    self.addNewButton.clicked.connect(self.loadImages)
    self.saveButton.clicked.connect(self.saveDB)
    self.loadButton.clicked.connect(self.loadDB)

    # Input image handler
    self.findButton.clicked.connect(self.findSuspect)




  # Skip using '.ui' all the time!
  def __getattr__(self, name):
    return self.ui.__getattribute__(name)


  # Load images to be added the photo database grid AND hopfield network
  def loadImages(self):
    filenames=openFile(self,
        nameFilter="Image files (*.jpg *.jpeg *.png *.bmp)",
        fileMode=QFileDialog.ExistingFiles)

    # if no files selected, abort
    if not filenames: return


    # Image importer wizard
    wizard = QWizard(self)
    pageIds = xrange(len(filenames))
    origAccept = wizard.accept


    # Handler function to add images to database
    def addImages():
      for i in pageIds:
        path = filenames[i]
        name = wizard.field('name-%d'%i)
        age = wizard.field('age-%d'%i)
        desc = wizard.field('desc-%d'%i)

        # Add images to grid
        self.addImageToDB(path, name, age, desc)

      return origAccept()

    # Set handler functions
    wizard.accept = addImages

    # Create and add wizard pages
    for (i, filename) in enumerate(filenames):
      page = ControlImageAddPage(filename)
      page.setCommitPage(True)
      page.setButtonText(QWizard.CommitButton, "Add")

      page.registerField('name-%d*'%i, page.nameInput)
      page.registerField('age-%d*'%i, page.ageInput)
      page.registerField("desc-%d"%i, page.descInput, "plainText");

      wizard.addPage(page)

    wizard.show()


  # Add image to photo database grid
  def addImageToDB(self, path, name, age="N/A", desc="None available"):
    # Create icon and item
    icon = QIcon(path)
    item = QListWidgetItem(icon, name)

    # Associated image data
    data = {pathKey: path, nameKey: name, ageKey:age, descKey:desc}
    item.setData(Qt.UserRole, data)

    # Add image to grid
    self.imageList.addItem(item)


  def getItemData(self):
    items = self.imageList.findItems('*', Qt.MatchWildcard)
    return [ item.data(Qt.UserRole) for item in items ]


  # Save DB to file - if none specified, ask user
  def saveDB(self, filename=None):
    # Retrieve all image data
    itemsData = self.getItemData()


    # If no file, ask user for file
    if filename is None:
      filename = saveFile(self, "Database files (*.db)", 'db')

    # If user selected nothing, abort
    if filename is None: return


    # Save to file
    try:
      with open(filename, 'w') as f:
        json.dump(itemsData, f)
        message(title='Success',
          message='File %s saved successfully!'%basename(filename))
    except IOError, e:
      message(title='Save error',
        icon=QMessageBox.Warning,
        message="An error has occurred while saving the file.",
        detail=str(e))
      return


  # Load image to match
  def findSuspect(self):
    imagePath=openFile(self,
        nameFilter="Image files (*.jpg *.jpeg *.png *.bmp)",
        fileMode=QFileDialog.ExistingFile)

    # If no image selected, abort
    if imagePath is None: return

    originalText = self.findButton.text()

    self.findButton.setText("Please wait...")
    self.findButton.setEnabled(False)

    # Set image
    self.inputImage.setPixmap(QPixmap(imagePath))

    self.matchImage(imagePath)



  def getAllStoredPaths(self):
    data = self.getItemData()
    return [d[pathKey] for d in data]


  def matchImage(self, imagePath):
    current_path = os.path.dirname(os.path.abspath(__file__))
    dist_path = "/../dist/build/recognize/"
    gui_path = "../../../gui/"
    exec_path = current_path + dist_path
    # this is dangerous. paths are re*lative to this gui thing
    storedImagesPaths = self.getAllStoredPaths()
    storedImagesPaths = [gui_path + p for p in storedImagesPaths]
    current_env = os.environ.copy()
    current_env["PATH"] += ":" + current_path
    current_env["PATH"] += ":" + exec_path
    proc = subprocess.Popen(
      ["recognize", "run", "hopfield",  "15", "15", imagePath] + storedImagesPaths,
      env= current_env, cwd=exec_path, stdout=subprocess.PIPE)

    self.rhsRec.setPixmap(QPixmap(None))
    self.nameOut.setText("")
    self.ageOut.setText("")
    self.descOut.setText("")

    # Note this is defined *within* match image to execute in a different thread
    def update(possible_path):
      name = ""
      age = ""
      description = ""

      try:
        if possible_path is None:
          raise Exception('The recognition sub-process encountered an error or was terminated unexpectedly.')

        elif possible_path.startswith(gui_path):
          actual_path = possible_path[len(gui_path):].strip()
          self.rhsRec.setPixmap(QPixmap(actual_path))
          for d in self.getItemData():
            if d[pathKey] == actual_path:
              name = d[nameKey]
              age = d[ageKey]
              description = d[descKey]

        else:
              QtGui.QMessageBox.information(self,"Suspect not found","The person you are trying to find is not in the database", QtGui.QMessageBox.Ok)

      except Exception, e:
        message(title='Matching error',
        icon=QMessageBox.Warning,
        message="An error has occurred while matching the image.",
        detail=str(e))

        name = ""
        age = ""
        description = ""

      # Ensure that if anything goes wrong, the button is re-enabled
      finally:
        self.nameOut.setText(name)
        self.ageOut.setText(age)
        self.descOut.setText(description)

        self.findButton.setText("Find suspect")
        self.findButton.setEnabled(True)

    # Run matching in separate thread
    class Future(QThread):
      dataReady = Signal(object)

      def run(self):
        # Wait for process to terminate and read output
        possible_path = proc.stdout.read()
        self.dataReady.emit(possible_path)


    self.finderThread = Future()
    self.finderThread.dataReady.connect(update, Qt.QueuedConnection)
    self.finderThread.start()



  # Load DB from file - if none specified, ask user
  def loadDB(self, filename=None):

    # If no file, ask user for file
    if filename is None:
      filename=openFile(self, nameFilter="Database files (*.db)",
        fileMode=QFileDialog.ExistingFile)

    # If user selected nothing, abort
    if filename is None: return


    try:
      with open(filename, 'r') as f:
        itemsData = json.load(f)
    except IOError, e:
      message(title='Load error',
        icon=QMessageBox.Warning,
        message="An error has occurred while loading the file.",
        detail=str(e))
      return


    self.clearDB()

    for itemData in itemsData:
      self.addImageToDB(**itemData)


  # Clear DB from memory - remove images from grid and their associated data
  def clearDB(self):
    self.imageList.clear()


  # Handler for when an image in the grid is selected
  @QtCore.Slot(QItemSelection, QItemSelection)
  def gridImageSelected(self, selected, deselected):
    selectedIndices = selected.indexes()

    if not selectedIndices:
      self.imageLabel.setPixmap(None)

      self.nameLabel.setText("")
      self.ageLabel.setText("")
      self.descLabel.setText("")

    else:
      selectedItem = selectedIndices[0]
      icon = selectedItem.data(Qt.DecorationRole)

      # Retrieve criminal data
      criminalData = selectedItem.data(Qt.UserRole)

      # Set image icon
      pixmap = icon.pixmap(ControlPoliceDB.FULL_IMAGE_SIZE)
      self.imageLabel.setPixmap(pixmap)

      # Set labels
      self.nameLabel.setText(criminalData['name'])
      self.ageLabel.setText(criminalData['age'])
      self.descLabel.setText(criminalData['desc'])


# Sample function - probably to be removed later
def loadImages(app):
  for (path, name, age, desc) in [("celeb.jpg", "Charlie Seen", '37', "Shady character"), ("mugchar.jpg", "Charlie Dude", "27", ' '.join('men'*200))]:
    app.addImageToDB(path, name, age, desc)


# Create message box with the specified message
def message(message, title, detail="", icon=QMessageBox.Information):
  msgBox = QMessageBox(icon, title, message)
  msgBox.setInformativeText(detail)
  msgBox.exec_()


# nameFilter is a string in the form:
# All C++ files (*.cpp *.cc *.C *.cxx *.c++)
# fileMode is one of:
# 1) QFileDialog.ExistingFile
# 2) QFileDialog.ExistingFiles
def openFile(parent, nameFilter, fileMode=QFileDialog.ExistingFile):
  dialog = QFileDialog(parent)
  dialog.setFileMode(fileMode)
  dialog.setNameFilter(nameFilter)

  # Return None if no files selected
  if not dialog.exec_(): return None
  fileNames = dialog.selectedFiles()

  if not fileNames:
    return None
  elif fileMode == QFileDialog.ExistingFile:
    return fileNames[0]
  elif fileMode == QFileDialog.ExistingFiles:
    return fileNames
  else:
    return None


# nameFilter is a string in the form:
# All C++ files (*.cpp *.cc *.C *.cxx *.c++)
# defaultExt is in the form:
# 'cpp'  (i.e. without the 'dot'!)
def saveFile(parent, nameFilter, defaultExt):
  dialog = QFileDialog(parent)
  dialog.setAcceptMode(QFileDialog.AcceptSave)
  dialog.setFileMode(QFileDialog.AnyFile)
  dialog.setNameFilter(nameFilter)
  dialog.setConfirmOverwrite(True)
  dialog.setDefaultSuffix(defaultExt)

  if not dialog.exec_(): return
  fileNames = dialog.selectedFiles()

  return fileNames[0] if fileNames else None



if __name__ == "__main__":
  app = QtGui.QApplication(sys.argv)
  mySW = ControlPoliceDB()
  mySW.loadDB('suspects.db')
  mySW.show()
  sys.exit(app.exec_())
