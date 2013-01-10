default: policedb.py policedbresource1_rc.py


clean:
	rm policedb.py policedbresource1_rc.py *.pyc

policedb.py: policedb.ui
	pyside-uic policedb.ui -o policedb.py

policedbresource1_rc.py: policedbresource.qrc
	pyside-rcc policedbresource.qrc -o policedbresource1_rc.py
