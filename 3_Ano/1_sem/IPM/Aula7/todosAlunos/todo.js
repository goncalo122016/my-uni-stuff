const app = Vue.createApp({
  data() {
    return {
      todos: [
        'Learn a new course',
        'Read a book',
        'Go to the gym',
        'Go shopping'
      ],
      today: new Date().toLocaleDateString(),
      enteredTodo: '',
    }
  },
  methods: {
   submitTodo() {
      if (this.todos.includes(this.enteredTodo)) {
        alert('This todo already exists!');
        return;
      }
      if (this.enteredTodo.trim() !== '') {
        this.todos.push(this.enteredTodo);
        this.enteredTodo = '';
      }
    },
    deleteTodo(i) {
      this.todos.splice(i, 1);
    },
    isValid() {
      return this.enteredTodo.trim() == '' ? true : false;
    }
  }
});

app.mount('#app');
