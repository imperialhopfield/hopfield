default: policedb.gen.py policedbresource.gen.py


policedb.gen.py: policedb.ui
	pyside-uic policedb.ui -o policedb.gen.py

policedbresource.gen.py: policedbresource.qrc
	pyside-rcc policedbresource.qrc -o policedbresource.gen.py
