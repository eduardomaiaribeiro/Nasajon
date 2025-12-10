.PHONY: help install run test clean example

help:
	@echo "Available commands:"
	@echo "  make install  - Install dependencies"
	@echo "  make run      - Run the application"
	@echo "  make test     - Run tests"
	@echo "  make example  - Run example usage script (requires server running)"
	@echo "  make clean    - Clean up generated files"

install:
	pip install -r requirements.txt

run:
	FLASK_DEBUG=true python app.py

test:
	pytest test_app.py -v

example:
	python example_usage.py

clean:
	rm -rf __pycache__ .pytest_cache
	find . -type f -name "*.pyc" -delete
	find . -type d -name "__pycache__" -delete
