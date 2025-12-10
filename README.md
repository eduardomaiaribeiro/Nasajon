# Nasajon - Teste para Contratação

## 📝 Descrição do Projeto / Project Description

Este é um teste de contratação que implementa uma API RESTful simples para gerenciamento de tarefas (Task Management System). A aplicação demonstra conhecimentos em desenvolvimento backend, criação de APIs REST, e boas práticas de programação.

*This is a hiring test that implements a simple RESTful API for task management. The application demonstrates backend development skills, REST API creation, and programming best practices.*

## 🚀 Tecnologias Utilizadas / Technologies Used

- **Python 3.x**
- **Flask** - Framework web minimalista para Python
- **pytest** - Framework de testes para Python

## 📋 Funcionalidades / Features

A API oferece as seguintes funcionalidades:

- ✅ Criar nova tarefa (CREATE)
- ✅ Listar todas as tarefas (READ)
- ✅ Buscar tarefa por ID (READ)
- ✅ Atualizar tarefa existente (UPDATE)
- ✅ Deletar tarefa (DELETE)
- ✅ Health check endpoint

## 🔧 Instalação / Installation

### Pré-requisitos / Prerequisites

- Python 3.7 ou superior
- pip (gerenciador de pacotes Python)

### Passos / Steps

1. Clone o repositório:
```bash
git clone https://github.com/eduardomaiaribeiro/Nasajon.git
cd Nasajon
```

2. Instale as dependências:
```bash
pip install -r requirements.txt
```

## ▶️ Executando a Aplicação / Running the Application

Para iniciar o servidor:
```bash
python app.py
```

O servidor estará disponível em: `http://localhost:5000`

## 🧪 Executando os Testes / Running Tests

Para executar os testes:
```bash
pytest test_app.py -v
```

Para executar com cobertura de código:
```bash
pytest test_app.py -v --cov=app
```

## 📚 Documentação da API / API Documentation

### Base URL
```
http://localhost:5000/api
```

### Endpoints

#### 1. Health Check
Verifica se a API está funcionando.

**GET** `/api/health`

**Resposta:**
```json
{
  "success": true,
  "status": "healthy",
  "timestamp": "2024-01-01T12:00:00.000000"
}
```

#### 2. Listar Todas as Tarefas / List All Tasks
**GET** `/api/tasks`

**Resposta:**
```json
{
  "success": true,
  "data": [
    {
      "id": "uuid-string",
      "title": "Tarefa de Exemplo",
      "description": "Descrição da tarefa",
      "status": "pending",
      "created_at": "2024-01-01T12:00:00.000000",
      "updated_at": "2024-01-01T12:00:00.000000"
    }
  ],
  "count": 1
}
```

#### 3. Buscar Tarefa por ID / Get Task by ID
**GET** `/api/tasks/{task_id}`

**Resposta de Sucesso:**
```json
{
  "success": true,
  "data": {
    "id": "uuid-string",
    "title": "Tarefa de Exemplo",
    "description": "Descrição da tarefa",
    "status": "pending",
    "created_at": "2024-01-01T12:00:00.000000",
    "updated_at": "2024-01-01T12:00:00.000000"
  }
}
```

**Resposta de Erro (404):**
```json
{
  "success": false,
  "error": "Task not found"
}
```

#### 4. Criar Nova Tarefa / Create New Task
**POST** `/api/tasks`

**Body:**
```json
{
  "title": "Nova Tarefa",
  "description": "Descrição opcional",
  "status": "pending"
}
```

**Resposta (201):**
```json
{
  "success": true,
  "data": {
    "id": "uuid-string",
    "title": "Nova Tarefa",
    "description": "Descrição opcional",
    "status": "pending",
    "created_at": "2024-01-01T12:00:00.000000",
    "updated_at": "2024-01-01T12:00:00.000000"
  }
}
```

#### 5. Atualizar Tarefa / Update Task
**PUT** `/api/tasks/{task_id}`

**Body:**
```json
{
  "title": "Título Atualizado",
  "description": "Descrição Atualizada",
  "status": "completed"
}
```

**Resposta (200):**
```json
{
  "success": true,
  "data": {
    "id": "uuid-string",
    "title": "Título Atualizado",
    "description": "Descrição Atualizada",
    "status": "completed",
    "created_at": "2024-01-01T12:00:00.000000",
    "updated_at": "2024-01-01T12:30:00.000000"
  }
}
```

#### 6. Deletar Tarefa / Delete Task
**DELETE** `/api/tasks/{task_id}`

**Resposta (200):**
```json
{
  "success": true,
  "data": {
    "id": "uuid-string",
    "title": "Tarefa Deletada",
    "description": "Esta tarefa foi removida",
    "status": "pending",
    "created_at": "2024-01-01T12:00:00.000000",
    "updated_at": "2024-01-01T12:00:00.000000"
  },
  "message": "Task deleted successfully"
}
```

## 🎯 Status Possíveis / Possible Statuses

- `pending` - Tarefa pendente
- `in_progress` - Tarefa em andamento
- `completed` - Tarefa concluída
- `cancelled` - Tarefa cancelada

## 📝 Exemplos de Uso / Usage Examples

### Usando cURL:

**Criar uma tarefa:**
```bash
curl -X POST http://localhost:5000/api/tasks \
  -H "Content-Type: application/json" \
  -d '{"title":"Minha Tarefa","description":"Descrição da tarefa","status":"pending"}'
```

**Listar tarefas:**
```bash
curl http://localhost:5000/api/tasks
```

**Atualizar tarefa:**
```bash
curl -X PUT http://localhost:5000/api/tasks/{task_id} \
  -H "Content-Type: application/json" \
  -d '{"status":"completed"}'
```

**Deletar tarefa:**
```bash
curl -X DELETE http://localhost:5000/api/tasks/{task_id}
```

## 🏗️ Arquitetura / Architecture

A aplicação segue uma arquitetura simples:

- **app.py** - Aplicação principal Flask com todos os endpoints
- **test_app.py** - Testes unitários e de integração
- **requirements.txt** - Dependências do projeto

## 🔒 Armazenamento / Storage

Atualmente, a aplicação utiliza armazenamento em memória (dicionário Python). Em um ambiente de produção, seria recomendado usar um banco de dados real como PostgreSQL, MongoDB, ou MySQL.

*Currently, the application uses in-memory storage (Python dictionary). In a production environment, it would be recommended to use a real database like PostgreSQL, MongoDB, or MySQL.*

## 🚧 Melhorias Futuras / Future Improvements

- [ ] Integração com banco de dados real
- [ ] Autenticação e autorização (JWT)
- [ ] Paginação para listagem de tarefas
- [ ] Filtros e busca avançada
- [ ] Validação de dados mais robusta
- [ ] Documentação Swagger/OpenAPI
- [ ] Docker containerization
- [ ] CI/CD pipeline

## 👨‍💻 Autor / Author

Eduardo Maia Ribeiro

## 📄 Licença / License

Este projeto é um teste de contratação e está disponível para fins educacionais.