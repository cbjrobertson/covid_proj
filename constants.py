# CV with transformers
CATS = ["HEALTHY_TALK","SICK_TALK"]
# MODEL_NAME = 'xlm-mlm-100-1280'
# MULTILINGUAL MODELS ON HUGGINGFACE, see: https://huggingface.co/docs/transformers/multilingual
# MODEL NAMES 

MODEL_NAMES = [
    # "xlm-mlm-100-1280",
    # "xlm-mlm-17-1280",
    # "bert-base-multilingual-uncased",
    # "bert-base-multilingual-cased",
    # "xlm-roberta-base",
    "xlm-roberta-large"
    ]
LEARN_RATE = 5e-5
EPOCHS = 12
BATCH = 12
TEXT_COL = "text"
LABEL_COL = "label"
CHK = "chkpnts"
MODELS="models"
RES = "results"
MET = "both"
EXP_NAME = "./save_model"
CMD = "sudo -S find /tmp/ -user coler -exec rm -fr {} \;"