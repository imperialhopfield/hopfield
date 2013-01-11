# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'imageaddpage.ui'
#
# Created: Fri Jan 11 01:21:16 2013
#      by: pyside-uic 0.2.13 running on PySide 1.1.2
#
# WARNING! All changes made in this file will be lost!

from PySide import QtCore, QtGui

class Ui_WizardPage(object):
    def setupUi(self, WizardPage):
        WizardPage.setObjectName("WizardPage")
        WizardPage.resize(545, 341)
        self.verticalLayout_2 = QtGui.QVBoxLayout(WizardPage)
        self.verticalLayout_2.setObjectName("verticalLayout_2")
        self.horizontalLayout = QtGui.QHBoxLayout()
        self.horizontalLayout.setObjectName("horizontalLayout")
        self.verticalLayout_4 = QtGui.QVBoxLayout()
        self.verticalLayout_4.setObjectName("verticalLayout_4")
        self.horizontalLayout_3 = QtGui.QHBoxLayout()
        self.horizontalLayout_3.setObjectName("horizontalLayout_3")
        self.label_2 = QtGui.QLabel(WizardPage)
        font = QtGui.QFont()
        font.setWeight(75)
        font.setBold(True)
        self.label_2.setFont(font)
        self.label_2.setObjectName("label_2")
        self.horizontalLayout_3.addWidget(self.label_2)
        self.nameInput = QtGui.QLineEdit(WizardPage)
        self.nameInput.setObjectName("nameInput")
        self.horizontalLayout_3.addWidget(self.nameInput)
        self.verticalLayout_4.addLayout(self.horizontalLayout_3)
        self.horizontalLayout_4 = QtGui.QHBoxLayout()
        self.horizontalLayout_4.setObjectName("horizontalLayout_4")
        self.label_31 = QtGui.QLabel(WizardPage)
        font = QtGui.QFont()
        font.setWeight(75)
        font.setBold(True)
        self.label_31.setFont(font)
        self.label_31.setObjectName("label_31")
        self.horizontalLayout_4.addWidget(self.label_31)
        self.ageInput = QtGui.QLineEdit(WizardPage)
        self.ageInput.setObjectName("ageInput")
        self.horizontalLayout_4.addWidget(self.ageInput)
        self.verticalLayout_4.addLayout(self.horizontalLayout_4)
        self.verticalLayout_3 = QtGui.QVBoxLayout()
        self.verticalLayout_3.setObjectName("verticalLayout_3")
        self.label_32 = QtGui.QLabel(WizardPage)
        font = QtGui.QFont()
        font.setWeight(75)
        font.setBold(True)
        self.label_32.setFont(font)
        self.label_32.setObjectName("label_32")
        self.verticalLayout_3.addWidget(self.label_32)
        self.descInput = QtGui.QTextEdit(WizardPage)
        self.descInput.setObjectName("descInput")
        self.verticalLayout_3.addWidget(self.descInput)
        self.verticalLayout_4.addLayout(self.verticalLayout_3)
        self.horizontalLayout.addLayout(self.verticalLayout_4)
        self.verticalLayout = QtGui.QVBoxLayout()
        self.verticalLayout.setObjectName("verticalLayout")
        self.imageLabel = QtGui.QLabel(WizardPage)
        self.imageLabel.setMinimumSize(QtCore.QSize(201, 201))
        self.imageLabel.setMaximumSize(QtCore.QSize(251, 241))
        self.imageLabel.setText("")
        self.imageLabel.setPixmap(QtGui.QPixmap(":/new/prefix1/lucasss.png"))
        self.imageLabel.setScaledContents(True)
        self.imageLabel.setObjectName("imageLabel")
        self.verticalLayout.addWidget(self.imageLabel)
        self.filePath = QtGui.QLineEdit(WizardPage)
        self.filePath.setReadOnly(True)
        self.filePath.setObjectName("filePath")
        self.verticalLayout.addWidget(self.filePath)
        self.horizontalLayout.addLayout(self.verticalLayout)
        self.verticalLayout_2.addLayout(self.horizontalLayout)

        self.retranslateUi(WizardPage)
        QtCore.QMetaObject.connectSlotsByName(WizardPage)

    def retranslateUi(self, WizardPage):
        WizardPage.setWindowTitle(QtGui.QApplication.translate("WizardPage", "WizardPage", None, QtGui.QApplication.UnicodeUTF8))
        WizardPage.setTitle(QtGui.QApplication.translate("WizardPage", "Suspect details", None, QtGui.QApplication.UnicodeUTF8))
        self.label_2.setText(QtGui.QApplication.translate("WizardPage", "Name:", None, QtGui.QApplication.UnicodeUTF8))
        self.nameInput.setPlaceholderText(QtGui.QApplication.translate("WizardPage", "enter name", None, QtGui.QApplication.UnicodeUTF8))
        self.label_31.setText(QtGui.QApplication.translate("WizardPage", "Age:", None, QtGui.QApplication.UnicodeUTF8))
        self.ageInput.setPlaceholderText(QtGui.QApplication.translate("WizardPage", "enter age", None, QtGui.QApplication.UnicodeUTF8))
        self.label_32.setText(QtGui.QApplication.translate("WizardPage", "Description:", None, QtGui.QApplication.UnicodeUTF8))

import policedbresource_rc
