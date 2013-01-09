import sys
from PySide import QtCore, QtGui
from policedb import Ui_PoliceDB

#  Runs the policeUI gui

class ControlPoliceDB(QtGui.QMainWindow):
  def __init__(self, parent=None):
      super(ControlPoliceDB, self).__init__(parent)
      self.ui =  Ui_PoliceDB()
      self.ui.setupUi(self)

if __name__ == "__main__":
    app = QtGui.QApplication(sys.argv)
    mySW = ControlPoliceDB()
    mySW.show()
    sys.exit(app.exec_())
