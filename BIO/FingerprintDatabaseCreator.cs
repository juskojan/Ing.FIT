using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.IO;
using BIO.Framework.Core.Database;
using BIO.Framework.Extensions.Standard.Database.InputDatabase;

namespace BIO.Project.FingerprintRecognition
{
    class FingerprintDatabaseCreator : IDatabaseCreator<StandardRecord<StandardRecordData>>
    {
        string pathToDatabase;

        public FingerprintDatabaseCreator(string path)
        {
            this.pathToDatabase = path;
        }

        public Database<StandardRecord<StandardRecordData>> createDatabase()
        {
            Database<StandardRecord<StandardRecordData>> database = new Database<StandardRecord<StandardRecordData>>();
            DirectoryInfo di = new DirectoryInfo(this.pathToDatabase);
            FileInfo[] files = di.GetFiles("*.png", SearchOption.AllDirectories);

            foreach(FileInfo file in files)
            {
                string[] parts = file.Name.Split(new char[] { '_' }, StringSplitOptions.RemoveEmptyEntries);
                BiometricID biometricID = new BiometricID(parts[0], "fingerprint");            
                StandardRecordData recordData = new StandardRecordData(file.FullName);
                StandardRecord<StandardRecordData> record = new StandardRecord<StandardRecordData>(file.Name, biometricID, recordData);

                database.addRecord(record);            
            }

            return database;
        }
    }
}
