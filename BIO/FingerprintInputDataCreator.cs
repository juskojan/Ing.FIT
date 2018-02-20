using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using BIO.Framework.Extensions.Emgu.InputData;
using BIO.Framework.Core.InputData;
using BIO.Framework.Extensions.Standard.Database.InputDatabase;

namespace BIO.Project.FingerprintRecognition
{
    public class FingerprintInputDataCreator : IInputDataCreator<StandardRecord<StandardRecordData>, EmguGrayImageInputData>
    {
        public EmguGrayImageInputData createInputData(StandardRecord<StandardRecordData> record)
        {
            return new EmguGrayImageInputData(record.BiometricData.Data);
        }
    }
}
