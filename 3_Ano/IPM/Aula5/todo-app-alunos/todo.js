const todos = [
  'Learn a new course',
  'Read a book',
  'Go to the gym',
  'Go shopping'
];

/* Ex. 1: Add a event listener that triggers when the DOM is loaded here */
document.addEventListener('DOMContentLoaded', () => {
  const dataAtual = new Date();
  document.getElementById('list-date').innerText = dataAtual.toLocaleDateString('pt-PT', { year: 'numeric', month: 'long', day: 'numeric' });
  renderTodoList();
});


/* Ex. 2: Complete todo rendering */
// complete function to remove all child nodes
// arg parent is the node to clean
function removeAllChildNodes(parent) {
  while (parent.firstChild) {
    parent.removeChild(parent.firstChild);
  }
}


// render todo array here
function renderTodoList() {
  todos.forEach((todo) => {
    const todoList = document.getElementById('todo-list');

    const li = document.createElement('li');
    li.classList.add('todo-list-item');

    const p = document.createElement('p');
    p.innerText = todo;
    li.appendChild(p);

    const deleteButton = document.createElement('button');
    deleteButton.classList.add = 'delete-button';
    deleteButton.innerText = 'Delete';
    deleteButton.addEventListener('click', removeTodoItem);
    li.appendChild(deleteButton);

    todoList.appendChild(li);
  });
}



/* Ex. 3: Add a event listener to element 'todo-form'*/
document.getElementById('todo-form').addEventListener('submit', event => {
  const newTask = document.getElementById('task-input').value;
  event.preventDefault();
  if (!newTask || newTask.trim() === '') {
    return;
  }
  if (todos.includes(newTask)) {
    alert('Task already exists!');
    return;
  }
  todos.push(newTask);
  removeAllChildNodes(document.getElementById('todo-list'));
  renderTodoList();
  document.getElementById('todo-form').reset();
});




/* Ex. 4 and 5: complete delete button click logic */
// arg event is the triggered event (with event you can get the element clicked).
function removeTodoItem(event) {
  todos.splice(todos.indexOf(event.target.previousSibling.innerText), 1);
  event.target.removeEventListener('click', removeTodoItem);
  removeAllChildNodes(document.getElementById('todo-list'));
  renderTodoList();
}