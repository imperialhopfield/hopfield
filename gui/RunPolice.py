import sys
from PySide import QtCore, QtGui
from policedb import Ui_PoliceDB

QIcon = QtGui.QIcon
QListWidgetItem = QtGui.QListWidgetItem
QListView = QtGui.QListView
QSize = QtCore.QSize


#  Runs the policeUI gui

class ControlPoliceDB(QtGui.QMainWindow):


	# Size of icons in
	ICON_SIZE = QSize(200, 200)

	def __init__(self, parent=None):
		super(ControlPoliceDB, self).__init__(parent)
		self.ui =  Ui_PoliceDB()
		self.ui.setupUi(self)

		self.imageList.setIconSize(ControlPoliceDB.ICON_SIZE)


	# Skip using '.ui' all the time!
	def __getattr__(self, name):
		return self.ui.__getattribute__(name)


	# Add image to photo database grid
	def addImageToGrid(self, path, name):
		icon = QIcon(path)
		item = QListWidgetItem(icon, name)
		self.imageList.addItem(item)



# Sample function - probably to be removed later
def loadImages(app):
	for (path, name) in [("celeb.jpg", "Charlie Seen"), ("mugchar.jpg", "Charlie Dude")]:
		app.addImageToGrid(path, name)



if __name__ == "__main__":
    app = QtGui.QApplication(sys.argv)
    mySW = ControlPoliceDB()
    loadImages(mySW)
    mySW.show()
    sys.exit(app.exec_())
