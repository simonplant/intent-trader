.PHONY: install install-dev test lint format clean docs

# Python interpreter
PYTHON = python3
VENV = venv
VENV_BIN = $(VENV)/bin

# Install production dependencies
install:
	$(PYTHON) -m pip install -r requirements.txt

# Install development dependencies
install-dev: install
	$(PYTHON) -m pip install -r dev-requirements.txt

# Create virtual environment
venv:
	$(PYTHON) -m venv $(VENV)
	$(VENV_BIN)/pip install --upgrade pip

# Run tests
test:
	pytest tests/ -v --cov=src

# Run linting
lint:
	flake8 src/ tests/
	mypy src/ tests/
	black --check src/ tests/
	isort --check-only src/ tests/

# Format code
format:
	black src/ tests/
	isort src/ tests/

# Clean up
clean:
	rm -rf build/
	rm -rf dist/
	rm -rf *.egg-info
	find . -type d -name __pycache__ -exec rm -rf {} +
	find . -type f -name "*.pyc" -delete
	find . -type f -name "*.pyo" -delete
	find . -type f -name "*.pyd" -delete
	find . -type f -name ".coverage" -delete
	find . -type d -name "*.egg" -exec rm -rf {} +
	find . -type d -name ".pytest_cache" -exec rm -rf {} +
	find . -type d -name ".mypy_cache" -exec rm -rf {} +
	find . -type d -name ".coverage" -exec rm -rf {} +

# Build documentation
docs:
	cd docs && make html

# Run pre-commit hooks
pre-commit:
	pre-commit run --all-files

# Setup development environment
setup-dev: venv install-dev
	pre-commit install

# Help
help:
	@echo "Available commands:"
	@echo "  make install        - Install production dependencies"
	@echo "  make install-dev    - Install development dependencies"
	@echo "  make test          - Run tests"
	@echo "  make lint          - Run linting"
	@echo "  make format        - Format code"
	@echo "  make clean         - Clean up build artifacts"
	@echo "  make docs          - Build documentation"
	@echo "  make pre-commit    - Run pre-commit hooks"
	@echo "  make setup-dev     - Setup development environment" 