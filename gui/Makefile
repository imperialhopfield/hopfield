default: policedb.py policedbresource_rc.py


clean:
	rm policedb.py policedbresource_rc.py *.pyc

policedb.py: policedb.ui
	pyside-uic policedb.ui -o policedb.py

policedbresource_rc.py: policedbresource.qrc
	pyside-rcc policedbresource.qrc -o policedbresource_rc.py
