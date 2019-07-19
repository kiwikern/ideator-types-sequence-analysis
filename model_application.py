import sys
import numpy as np
import pickle
import json

if __name__=='__main__':

    script_name       = sys.argv[0]
    model_filename    = sys.argv[1]
    classify_filename = sys.argv[2]

    model = pickle.load(open(model_filename, 'rb'))
    x_toclassify = np.array(json.loads(classify_filename))
    if len(x_toclassify.shape)==1:
        y_pred = model.predict([x_toclassify])
    else:
        y_pred = model.predict(x_toclassify)

    sys.stdout.write(str(y_pred))
    sys.stdout.flush()
    sys.exit(0)
    #y_pred = [0 if val<0.5 else 1 for val in y_pred]
    #print(y_pred)

#[0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
#[1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
