let game;

document.addEventListener('DOMContentLoaded', () => {
    game = new Game();
});

document.getElementById('resetButton').addEventListener('click', () => {
    game.reset();
});

class Game {
    constructor() {
        this.board = ['', '', '', '', '', '', '', '', ''];
        this.currentPlayer = 'X';
        this.isGameOver = false;
        this.init();
    }

    init() {
        this.createBoard();
        this.updateStatus(`Vez do Jogador ${this.currentPlayer}`);
    }

    reset() {
        this.board = ['', '', '', '', '', '', '', '', ''];
        this.isGameOver = false;
        this.currentPlayer = 'X';
        this.createBoard();
        this.updateStatus(`Vez do Jogador ${this.currentPlayer}`);
    }

    createBoard() {
        const boardContainer = document.getElementById('gameBoard');
        this.board.forEach((cellValue, index) => {
            const cell = boardContainer.children[index];
            cell.innerText = cellValue;
            cell.className = 'cell'; 
            if (cellValue === 'X') {
                cell.classList.add('x');
            } else if (cellValue === 'O') {
                cell.classList.add('o');
            }
            cell.onclick = () => this.handleCellClick(index);
        });
    }

    updateStatus(message) {
        const playerTurn = document.getElementById('playerTurn');
        playerTurn.innerText = message;
    }

    handleCellClick(index) {
        if (this.board[index] || this.isGameOver) return;

        this.board[index] = this.currentPlayer;
        this.createBoard();
        this.checkForWinner();
        
        if (!this.isGameOver) {
            this.currentPlayer = this.currentPlayer === 'X' ? 'O' : 'X';
            this.updateStatus(`Vez do Jogador ${this.currentPlayer}`);
        }
    }

    checkForWinner() {
        const winningCombinations = [
            [0, 1, 2],
            [3, 4, 5],
            [6, 7, 8],
            [0, 3, 6],
            [1, 4, 7],
            [2, 5, 8],
            [0, 4, 8],
            [2, 4, 6]
        ];

        for (const combination of winningCombinations) {
            const [a, b, c] = combination;
            if (this.board[a] && this.board[a] === this.board[b] && this.board[a] === this.board[c]) {
                this.isGameOver = true;
                this.updateStatus(`Jogador ${this.board[a]} venceu!`);
                return;
            }
        }

        if (!this.board.includes('')) {
            this.isGameOver = true;
            this.updateStatus('Empate!');
        }
    }
}
