#include "policedb.h"
#include "ui_policedb.h"

PoliceDB::PoliceDB(QWidget *parent) :
    QMainWindow(parent),
    ui(new Ui::PoliceDB)
{
    ui->setupUi(this);
}

PoliceDB::~PoliceDB()
{
    delete ui;
}
