import sys
from os.path import basename
from PySide import QtCore, QtGui
from policedb import Ui_PoliceDB
import json

QIcon = QtGui.QIcon
QListWidgetItem = QtGui.QListWidgetItem
QListView = QtGui.QListView
QMessageBox = QtGui.QMessageBox
QItemSelection = QtGui.QItemSelection
QFileDialog = QtGui.QFileDialog
QSize = QtCore.QSize
Qt = QtCore.Qt



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
		self.addNewButton.clicked.connect(self.addPattern)
		self.saveButton.clicked.connect(self.saveDB)
		self.loadButton.clicked.connect(self.loadDB)



	# Skip using '.ui' all the time!
	def __getattr__(self, name):
		return self.ui.__getattribute__(name)


	def addPattern(self):
		print "Hello ela!"


	# Add image to photo database grid
	def addImageToDB(self, path, name, age="N/A", desc="None available"):
		# Create icon and item
		icon = QIcon(path)
		item = QListWidgetItem(icon, name)

		# Associated image data
		data = {'path': path, 'name': name, 'age':age, 'desc':desc}
		item.setData(Qt.UserRole, data)

		# Add image to grid
		self.imageList.addItem(item)


	# Save DB to file - if none specified, ask user
	def saveDB(self, filename=None):
		# Retrieve all image data
		items = self.imageList.findItems('*', Qt.MatchWildcard)
		itemsData = [ item.data(Qt.UserRole) for item in items ]


		# If no file, ask user for file
		if filename is None:
			dialog = QFileDialog(self)
			dialog.setAcceptMode(QFileDialog.AcceptSave)
			dialog.setFileMode(QFileDialog.AnyFile)
			dialog.setNameFilter("Database files (*.db)")
			dialog.setConfirmOverwrite(True)
			dialog.setLabelText(QFileDialog.Accept, "Save")
			dialog.setDefaultSuffix("db")

			if not dialog.exec_(): return
			fileNames = dialog.selectedFiles()
			if not fileNames: return
			filename = fileNames[0]


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


	# Load DB from file - if none specified, ask user
	def loadDB(self, filename=None):

		# If no file, ask user for file
		if filename is None:
			dialog = QFileDialog(self)
			dialog.setFileMode(QFileDialog.ExistingFile)
			dialog.setNameFilter("Database files (*.db)")

			if not dialog.exec_(): return
			fileNames = dialog.selectedFiles()
			if not fileNames: return
			filename = fileNames[0]


		try:
			with open(filename, 'r') as f:
				itemsData = json.load(f)
		except IOError, e:
			message(title='Load error',
				icon=QMessageBox.Warning,
				message="An error has occurred while loading the file.",
				etail=str(e))


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


if __name__ == "__main__":
	app = QtGui.QApplication(sys.argv)
	mySW = ControlPoliceDB()
	mySW.loadDB('db')
	mySW.show()
	sys.exit(app.exec_())
