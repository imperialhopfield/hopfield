import sys
from PySide import QtCore, QtGui
from policedb import Ui_PoliceDB

QIcon = QtGui.QIcon
QListWidgetItem = QtGui.QListWidgetItem
QListView = QtGui.QListView
QItemSelection = QtGui.QItemSelection
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


	# Skip using '.ui' all the time!
	def __getattr__(self, name):
		return self.ui.__getattribute__(name)


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



if __name__ == "__main__":
    app = QtGui.QApplication(sys.argv)
    mySW = ControlPoliceDB()
    loadImages(mySW)
    mySW.show()
    sys.exit(app.exec_())
