# Machine Learning (CS MSc. Course) 

Collaborative repo for team projects of Machine Learning. Master in Computer Science at ITAM.


Project 1: 

Project 2: Forecast Reconciliation 

Es común encontrar datos de series de tiempo cuya organización refleja una estructura de jerarquía. Cualquier cadena u institución con presencia nacional o internacional debe considerar las delimitaciones geográficas en su planeación. Por ejemplo, la venta de un artículo se puede visualizar como su venta en una tienda específica, en un estado o en una región. Similarmente, dicho artículo puede ser parte de una categoría específica que a su vez es parte de una línea de producto, por lo que su venta debe estar reflejada en estos niveles. 

Una muestra de lo anterior se observa en la siguiente figura:

<img src="https://raw.githubusercontent.com/savrgg/MsCS_Machine_Learning/main/partial_2_forecast_reconciliation/imgs/jerar1.png" alt="Drawing" style="float: center;"/>


A este tipo de datos cuyo valor puede ser agregado y que refleja un orden temporal se le conoce como series de tiempo jerárquicas. Las series de tiempo jerárquicas tienen la dificultad adicional de conciliar los pronósticos, esto es, de garantizar que la suma de las predicciones individuales es equivalente a realizar una predicción del total general (podemos pronosticar la demanda de turistas en un hotel en una región y así para todo el país. La suma de estas debe ser equivalente al pronóstico individual de la demanda de la temporada de verano completa). El proceso de conciliación típicamente degrada la exactitud del pronóstico. En este proyecto se exploran distintas alternativas para el forecast reconciliation.

Para el proyecto se emplean las librerias: tidyverts, tsibble, fable y feasts. Como base de datos se emplea la base de datos de tourism, que contiene los viajes nocturnos trimestrales en Australia.

<img src="https://raw.githubusercontent.com/savrgg/MsCS_Machine_Learning/main/partial_2_forecast_reconciliation/imgs/03_ts_region_purpose.png" alt="Drawing" style="float: center;"/>


En la siguiente gráfica se muestran los resultados sobre un forecast reconciliated y uno no reconciliated:

<img src="https://raw.githubusercontent.com/savrgg/MsCS_Machine_Learning/main/partial_2_forecast_reconciliation/imgs/17_no_reconciled_forecast.png" alt="Drawing" style="float: center;"/>













