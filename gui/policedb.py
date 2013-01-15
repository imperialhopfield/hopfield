# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'policedb.ui'
#
# Created: Tue Jan 15 14:06:33 2013
#      by: pyside-uic 0.2.13 running on PySide 1.1.0
#
# WARNING! All changes made in this file will be lost!

from PySide import QtCore, QtGui

class Ui_PoliceDB(object):
    def setupUi(self, PoliceDB):
        PoliceDB.setObjectName("PoliceDB")
        PoliceDB.resize(787, 637)
        self.centralWidget = QtGui.QWidget(PoliceDB)
        self.centralWidget.setObjectName("centralWidget")
        self.verticalLayout = QtGui.QVBoxLayout(self.centralWidget)
        self.verticalLayout.setObjectName("verticalLayout")
        self.tabWidget = QtGui.QTabWidget(self.centralWidget)
        font = QtGui.QFont()
        font.setFamily("Helvetica LT Std Light")
        font.setPointSize(10)
        font.setWeight(50)
        font.setBold(False)
        self.tabWidget.setFont(font)
        self.tabWidget.setLayoutDirection(QtCore.Qt.LeftToRight)
        self.tabWidget.setTabPosition(QtGui.QTabWidget.North)
        self.tabWidget.setIconSize(QtCore.QSize(40, 40))
        self.tabWidget.setObjectName("tabWidget")
        self.suspect_database_tab = QtGui.QWidget()
        self.suspect_database_tab.setObjectName("suspect_database_tab")
        self.verticalLayout_2 = QtGui.QVBoxLayout(self.suspect_database_tab)
        self.verticalLayout_2.setObjectName("verticalLayout_2")
        self.label = QtGui.QLabel(self.suspect_database_tab)
        font = QtGui.QFont()
        font.setFamily("Helvetica LT Std Light")
        font.setPointSize(48)
        font.setWeight(50)
        font.setBold(False)
        self.label.setFont(font)
        self.label.setObjectName("label")
        self.verticalLayout_2.addWidget(self.label)
        self.horizontalLayout_2 = QtGui.QHBoxLayout()
        self.horizontalLayout_2.setObjectName("horizontalLayout_2")
        self.lhs = QtGui.QVBoxLayout()
        self.lhs.setObjectName("lhs")
        self.horizontalLayout = QtGui.QHBoxLayout()
        self.horizontalLayout.setObjectName("horizontalLayout")
        self.label_10 = QtGui.QLabel(self.suspect_database_tab)
        self.label_10.setObjectName("label_10")
        self.horizontalLayout.addWidget(self.label_10)
        self.addNewButton = QtGui.QToolButton(self.suspect_database_tab)
        icon = QtGui.QIcon()
        icon.addPixmap(QtGui.QPixmap(":/new/prefix1/icons/add.png"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        self.addNewButton.setIcon(icon)
        self.addNewButton.setIconSize(QtCore.QSize(42, 42))
        self.addNewButton.setAutoRaise(True)
        self.addNewButton.setObjectName("addNewButton")
        self.horizontalLayout.addWidget(self.addNewButton)
        self.saveButton = QtGui.QToolButton(self.suspect_database_tab)
        icon1 = QtGui.QIcon()
        icon1.addPixmap(QtGui.QPixmap(":/new/prefix1/icons/save.png"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        self.saveButton.setIcon(icon1)
        self.saveButton.setIconSize(QtCore.QSize(42, 42))
        self.saveButton.setAutoRaise(True)
        self.saveButton.setObjectName("saveButton")
        self.horizontalLayout.addWidget(self.saveButton)
        self.loadButton = QtGui.QToolButton(self.suspect_database_tab)
        icon2 = QtGui.QIcon()
        icon2.addPixmap(QtGui.QPixmap(":/new/prefix1/icons/open.png"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        self.loadButton.setIcon(icon2)
        self.loadButton.setIconSize(QtCore.QSize(42, 42))
        self.loadButton.setAutoRaise(True)
        self.loadButton.setObjectName("loadButton")
        self.horizontalLayout.addWidget(self.loadButton)
        self.lhs.addLayout(self.horizontalLayout)
        self.imageList = QtGui.QListWidget(self.suspect_database_tab)
        self.imageList.setDragDropMode(QtGui.QAbstractItemView.DragDrop)
        self.imageList.setMovement(QtGui.QListView.Free)
        self.imageList.setResizeMode(QtGui.QListView.Adjust)
        self.imageList.setLayoutMode(QtGui.QListView.Batched)
        self.imageList.setViewMode(QtGui.QListView.IconMode)
        self.imageList.setUniformItemSizes(True)
        self.imageList.setWordWrap(True)
        self.imageList.setObjectName("imageList")
        self.lhs.addWidget(self.imageList)
        self.horizontalLayout_2.addLayout(self.lhs)
        self.rhs = QtGui.QGroupBox(self.suspect_database_tab)
        self.rhs.setObjectName("rhs")
        self.gridLayout = QtGui.QGridLayout(self.rhs)
        self.gridLayout.setObjectName("gridLayout")
        self.verticalLayout_4 = QtGui.QVBoxLayout()
        self.verticalLayout_4.setObjectName("verticalLayout_4")
        self.horizontalLayout_3 = QtGui.QHBoxLayout()
        self.horizontalLayout_3.setObjectName("horizontalLayout_3")
        self.label_2 = QtGui.QLabel(self.rhs)
        font = QtGui.QFont()
        font.setWeight(75)
        font.setBold(True)
        self.label_2.setFont(font)
        self.label_2.setObjectName("label_2")
        self.horizontalLayout_3.addWidget(self.label_2)
        self.nameLabel = QtGui.QLabel(self.rhs)
        self.nameLabel.setText("")
        self.nameLabel.setTextInteractionFlags(QtCore.Qt.LinksAccessibleByMouse|QtCore.Qt.TextSelectableByKeyboard|QtCore.Qt.TextSelectableByMouse)
        self.nameLabel.setObjectName("nameLabel")
        self.horizontalLayout_3.addWidget(self.nameLabel)
        self.verticalLayout_4.addLayout(self.horizontalLayout_3)
        self.horizontalLayout_4 = QtGui.QHBoxLayout()
        self.horizontalLayout_4.setObjectName("horizontalLayout_4")
        self.label_31 = QtGui.QLabel(self.rhs)
        font = QtGui.QFont()
        font.setWeight(75)
        font.setBold(True)
        self.label_31.setFont(font)
        self.label_31.setObjectName("label_31")
        self.horizontalLayout_4.addWidget(self.label_31)
        self.ageLabel = QtGui.QLabel(self.rhs)
        self.ageLabel.setText("")
        self.ageLabel.setTextInteractionFlags(QtCore.Qt.LinksAccessibleByMouse|QtCore.Qt.TextSelectableByKeyboard|QtCore.Qt.TextSelectableByMouse)
        self.ageLabel.setObjectName("ageLabel")
        self.horizontalLayout_4.addWidget(self.ageLabel)
        self.verticalLayout_4.addLayout(self.horizontalLayout_4)
        self.verticalLayout_3 = QtGui.QVBoxLayout()
        self.verticalLayout_3.setObjectName("verticalLayout_3")
        self.label_32 = QtGui.QLabel(self.rhs)
        font = QtGui.QFont()
        font.setWeight(75)
        font.setBold(True)
        self.label_32.setFont(font)
        self.label_32.setObjectName("label_32")
        self.verticalLayout_3.addWidget(self.label_32)
        self.descLabel = QtGui.QTextBrowser(self.rhs)
        palette = QtGui.QPalette()
        brush = QtGui.QBrush(QtGui.QColor(242, 241, 240))
        brush.setStyle(QtCore.Qt.SolidPattern)
        palette.setBrush(QtGui.QPalette.Active, QtGui.QPalette.Base, brush)
        brush = QtGui.QBrush(QtGui.QColor(242, 241, 240))
        brush.setStyle(QtCore.Qt.SolidPattern)
        palette.setBrush(QtGui.QPalette.Inactive, QtGui.QPalette.Base, brush)
        brush = QtGui.QBrush(QtGui.QColor(255, 255, 255))
        brush.setStyle(QtCore.Qt.SolidPattern)
        palette.setBrush(QtGui.QPalette.Disabled, QtGui.QPalette.Base, brush)
        self.descLabel.setPalette(palette)
        self.descLabel.setHorizontalScrollBarPolicy(QtCore.Qt.ScrollBarAlwaysOff)
        self.descLabel.setTextInteractionFlags(QtCore.Qt.LinksAccessibleByKeyboard|QtCore.Qt.LinksAccessibleByMouse|QtCore.Qt.TextBrowserInteraction|QtCore.Qt.TextSelectableByKeyboard|QtCore.Qt.TextSelectableByMouse)
        self.descLabel.setObjectName("descLabel")
        self.verticalLayout_3.addWidget(self.descLabel)
        self.verticalLayout_4.addLayout(self.verticalLayout_3)
        self.gridLayout.addLayout(self.verticalLayout_4, 1, 0, 1, 1)
        self.imageLabel = QtGui.QLabel(self.rhs)
        self.imageLabel.setMinimumSize(QtCore.QSize(201, 201))
        self.imageLabel.setMaximumSize(QtCore.QSize(251, 241))
        self.imageLabel.setText("")
        self.imageLabel.setScaledContents(True)
        self.imageLabel.setObjectName("imageLabel")
        self.gridLayout.addWidget(self.imageLabel, 0, 0, 1, 1)
        self.horizontalLayout_2.addWidget(self.rhs)
        self.verticalLayout_2.addLayout(self.horizontalLayout_2)
        self.line = QtGui.QFrame(self.suspect_database_tab)
        self.line.setAcceptDrops(False)
        self.line.setAutoFillBackground(False)
        self.line.setFrameShadow(QtGui.QFrame.Sunken)
        self.line.setLineWidth(1)
        self.line.setFrameShape(QtGui.QFrame.HLine)
        self.line.setFrameShadow(QtGui.QFrame.Sunken)
        self.line.setObjectName("line")
        self.verticalLayout_2.addWidget(self.line)
        self.tabWidget.addTab(self.suspect_database_tab, "")
        self.suspect_matching_tab = QtGui.QWidget()
        self.suspect_matching_tab.setObjectName("suspect_matching_tab")
        self.verticalLayout_6 = QtGui.QVBoxLayout(self.suspect_matching_tab)
        self.verticalLayout_6.setObjectName("verticalLayout_6")
        self.label_4 = QtGui.QLabel(self.suspect_matching_tab)
        self.label_4.setMaximumSize(QtCore.QSize(16777215, 70))
        font = QtGui.QFont()
        font.setFamily("Helvetica LT Std Light")
        font.setPointSize(48)
        font.setWeight(50)
        font.setBold(False)
        self.label_4.setFont(font)
        self.label_4.setObjectName("label_4")
        self.verticalLayout_6.addWidget(self.label_4)
        self.line_2 = QtGui.QFrame(self.suspect_matching_tab)
        self.line_2.setAcceptDrops(False)
        self.line_2.setAutoFillBackground(False)
        self.line_2.setFrameShadow(QtGui.QFrame.Sunken)
        self.line_2.setLineWidth(1)
        self.line_2.setFrameShape(QtGui.QFrame.HLine)
        self.line_2.setFrameShadow(QtGui.QFrame.Sunken)
        self.line_2.setObjectName("line_2")
        self.verticalLayout_6.addWidget(self.line_2)
        self.verticalLayout_5 = QtGui.QVBoxLayout()
        self.verticalLayout_5.setObjectName("verticalLayout_5")
        self.horizontalLayout_5 = QtGui.QHBoxLayout()
        self.horizontalLayout_5.setObjectName("horizontalLayout_5")
        self.groupBox_4 = QtGui.QGroupBox(self.suspect_matching_tab)
        self.groupBox_4.setObjectName("groupBox_4")
        self.inputImage = QtGui.QLabel(self.groupBox_4)
        self.inputImage.setGeometry(QtCore.QRect(10, 20, 301, 301))
        self.inputImage.setText("")
        self.inputImage.setScaledContents(True)
        self.inputImage.setObjectName("inputImage")
        self.horizontalLayout_5.addWidget(self.groupBox_4)
        self.groupBox_5 = QtGui.QGroupBox(self.suspect_matching_tab)
        self.groupBox_5.setObjectName("groupBox_5")
        self.rhsRec = QtGui.QLabel(self.groupBox_5)
        self.rhsRec.setGeometry(QtCore.QRect(10, 20, 301, 301))
        self.rhsRec.setText("")
        self.rhsRec.setScaledContents(True)
        self.rhsRec.setObjectName("rhsRec")
        self.horizontalLayout_5.addWidget(self.groupBox_5)
        self.verticalLayout_5.addLayout(self.horizontalLayout_5)
        self.horizontalLayout_6 = QtGui.QHBoxLayout()
        self.horizontalLayout_6.setObjectName("horizontalLayout_6")
        self.label_3 = QtGui.QLabel(self.suspect_matching_tab)
        self.label_3.setMaximumSize(QtCore.QSize(16777215, 30))
        font = QtGui.QFont()
        font.setWeight(75)
        font.setBold(True)
        self.label_3.setFont(font)
        self.label_3.setObjectName("label_3")
        self.horizontalLayout_6.addWidget(self.label_3)
        self.nameOut = QtGui.QLabel(self.suspect_matching_tab)
        self.nameOut.setMaximumSize(QtCore.QSize(16777215, 30))
        self.nameOut.setText("")
        self.nameOut.setObjectName("nameOut")
        self.horizontalLayout_6.addWidget(self.nameOut)
        self.verticalLayout_5.addLayout(self.horizontalLayout_6)
        self.horizontalLayout_7 = QtGui.QHBoxLayout()
        self.horizontalLayout_7.setSpacing(0)
        self.horizontalLayout_7.setSizeConstraint(QtGui.QLayout.SetMaximumSize)
        self.horizontalLayout_7.setContentsMargins(-1, 0, -1, -1)
        self.horizontalLayout_7.setObjectName("horizontalLayout_7")
        self.label_6 = QtGui.QLabel(self.suspect_matching_tab)
        self.label_6.setMaximumSize(QtCore.QSize(16777215, 30))
        font = QtGui.QFont()
        font.setWeight(75)
        font.setBold(True)
        self.label_6.setFont(font)
        self.label_6.setObjectName("label_6")
        self.horizontalLayout_7.addWidget(self.label_6)
        self.ageOut = QtGui.QLabel(self.suspect_matching_tab)
        self.ageOut.setMaximumSize(QtCore.QSize(16777215, 30))
        self.ageOut.setText("")
        self.ageOut.setObjectName("ageOut")
        self.horizontalLayout_7.addWidget(self.ageOut)
        self.verticalLayout_5.addLayout(self.horizontalLayout_7)
        self.horizontalLayout_8 = QtGui.QHBoxLayout()
        self.horizontalLayout_8.setObjectName("horizontalLayout_8")
        self.label_8 = QtGui.QLabel(self.suspect_matching_tab)
        font = QtGui.QFont()
        font.setWeight(75)
        font.setBold(True)
        self.label_8.setFont(font)
        self.label_8.setObjectName("label_8")
        self.horizontalLayout_8.addWidget(self.label_8)
        self.descOut = QtGui.QTextBrowser(self.suspect_matching_tab)
        sizePolicy = QtGui.QSizePolicy(QtGui.QSizePolicy.Expanding, QtGui.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.descOut.sizePolicy().hasHeightForWidth())
        self.descOut.setSizePolicy(sizePolicy)
        self.descOut.setMaximumSize(QtCore.QSize(16777215, 100))
        self.descOut.setObjectName("descOut")
        self.horizontalLayout_8.addWidget(self.descOut)
        self.verticalLayout_5.addLayout(self.horizontalLayout_8)
        self.verticalLayout_6.addLayout(self.verticalLayout_5)
        self.findButton = QtGui.QPushButton(self.suspect_matching_tab)
        self.findButton.setMaximumSize(QtCore.QSize(125, 16777215))
        self.findButton.setObjectName("findButton")
        self.verticalLayout_6.addWidget(self.findButton)
        self.tabWidget.addTab(self.suspect_matching_tab, "")
        self.verticalLayout.addWidget(self.tabWidget)
        PoliceDB.setCentralWidget(self.centralWidget)
        self.statusBar = QtGui.QStatusBar(PoliceDB)
        self.statusBar.setObjectName("statusBar")
        PoliceDB.setStatusBar(self.statusBar)

        self.retranslateUi(PoliceDB)
        self.tabWidget.setCurrentIndex(0)
        QtCore.QMetaObject.connectSlotsByName(PoliceDB)

    def retranslateUi(self, PoliceDB):
        PoliceDB.setWindowTitle(QtGui.QApplication.translate("PoliceDB", "PoliceDB", None, QtGui.QApplication.UnicodeUTF8))
        self.label.setText(QtGui.QApplication.translate("PoliceDB", "Suspect Database", None, QtGui.QApplication.UnicodeUTF8))
        self.label_10.setText(QtGui.QApplication.translate("PoliceDB", "The following images are trained into the database:", None, QtGui.QApplication.UnicodeUTF8))
        self.addNewButton.setToolTip(QtGui.QApplication.translate("PoliceDB", "Add suspect", None, QtGui.QApplication.UnicodeUTF8))
        self.addNewButton.setText(QtGui.QApplication.translate("PoliceDB", "Add Suspect", None, QtGui.QApplication.UnicodeUTF8))
        self.saveButton.setToolTip(QtGui.QApplication.translate("PoliceDB", "<html><head/><body><p>Save database</p></body></html>", None, QtGui.QApplication.UnicodeUTF8))
        self.saveButton.setText(QtGui.QApplication.translate("PoliceDB", "Save Database", None, QtGui.QApplication.UnicodeUTF8))
        self.loadButton.setToolTip(QtGui.QApplication.translate("PoliceDB", "<html><head/><body><p>Load database</p></body></html>", None, QtGui.QApplication.UnicodeUTF8))
        self.loadButton.setText(QtGui.QApplication.translate("PoliceDB", "Save Database", None, QtGui.QApplication.UnicodeUTF8))
        self.rhs.setTitle(QtGui.QApplication.translate("PoliceDB", "Selected suspect", None, QtGui.QApplication.UnicodeUTF8))
        self.label_2.setText(QtGui.QApplication.translate("PoliceDB", "Name:", None, QtGui.QApplication.UnicodeUTF8))
        self.label_31.setText(QtGui.QApplication.translate("PoliceDB", "Age:", None, QtGui.QApplication.UnicodeUTF8))
        self.label_32.setText(QtGui.QApplication.translate("PoliceDB", "Description:", None, QtGui.QApplication.UnicodeUTF8))
        self.descLabel.setHtml(QtGui.QApplication.translate("PoliceDB", "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0//EN\" \"http://www.w3.org/TR/REC-html40/strict.dtd\">\n"
"<html><head><meta name=\"qrichtext\" content=\"1\" /><style type=\"text/css\">\n"
"p, li { white-space: pre-wrap; }\n"
"</style></head><body style=\" font-family:\'Helvetica LT Std Light\'; font-size:10pt; font-weight:400; font-style:normal;\">\n"
"<p style=\"-qt-paragraph-type:empty; margin-top:0px; margin-bottom:14px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><br /></p></body></html>", None, QtGui.QApplication.UnicodeUTF8))
        self.tabWidget.setTabText(self.tabWidget.indexOf(self.suspect_database_tab), QtGui.QApplication.translate("PoliceDB", "Suspect Database", None, QtGui.QApplication.UnicodeUTF8))
        self.label_4.setText(QtGui.QApplication.translate("PoliceDB", "Suspect Matching", None, QtGui.QApplication.UnicodeUTF8))
        self.groupBox_4.setTitle(QtGui.QApplication.translate("PoliceDB", "Input image", None, QtGui.QApplication.UnicodeUTF8))
        self.groupBox_5.setTitle(QtGui.QApplication.translate("PoliceDB", "Best matching image", None, QtGui.QApplication.UnicodeUTF8))
        self.label_3.setText(QtGui.QApplication.translate("PoliceDB", "Name:", None, QtGui.QApplication.UnicodeUTF8))
        self.label_6.setText(QtGui.QApplication.translate("PoliceDB", "Age:", None, QtGui.QApplication.UnicodeUTF8))
        self.label_8.setText(QtGui.QApplication.translate("PoliceDB", "Description:", None, QtGui.QApplication.UnicodeUTF8))
        self.findButton.setText(QtGui.QApplication.translate("PoliceDB", "Find suspect", None, QtGui.QApplication.UnicodeUTF8))
        self.tabWidget.setTabText(self.tabWidget.indexOf(self.suspect_matching_tab), QtGui.QApplication.translate("PoliceDB", "Suspect Matching", None, QtGui.QApplication.UnicodeUTF8))

import policedbresource_rc
