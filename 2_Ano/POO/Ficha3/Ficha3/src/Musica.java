public class Musica {
    private String name;
    private String artist;
    private String author;
    private String editor;
    private String[] lyrics;
    private String[] musicalNotes;
    private int duration;
    private int n;

    public int qtsLinhasPoema(){
        return lyrics.length;
    }

    public int numeroCarateres(){
        int res = 0;
        for (int i = 0; i < lyrics.length; i++){
            res += lyrics[i].length();
        }
        return res;
    }

    public String linhaMaisLonga(){
        String[] lyrics = this.getLyrics().clone();
        String res = lyrics[0];
        for (int i = 0; i < lyrics.length; i++){
            if (lyrics[i].length() > res.length()){
                res = lyrics[i];
            }
        }
        return res;
    }

    public void addLetra(int posicao, String novaLinha){
        String[] lyrics = this.getLyrics();
        lyrics[posicao] = novaLinha;
        this.setLyrics(lyrics);
    }

    public Musica() {
        this.name = "";
        this.artist = "";
        this.author = "";
        this.editor = "";
        this.lyrics = new String[0];
        this.musicalNotes = new String[0];
        this.duration = 0;
        this.n = 0;
    }

    public Musica(String name, String artist, String author, String editor, String[] lyrics, String[] musicalNotes, int duration, int n) {
        this.name = name;
        this.artist = artist;
        this.author = author;
        this.editor = editor;
        this.lyrics = lyrics;
        this.musicalNotes = musicalNotes;
        this.duration = duration;
        this.n = n;
    }

    public Musica(Musica m) {
        this.name = m.getName();
        this.artist = m.getArtist();
        this.author = m.getAuthor();
        this.editor = m.getEditor();
        this.lyrics = m.getLyrics();
        this.musicalNotes = m.getMusicalNotes();
        this.duration = m.getDuration();
        this.n = m.getN();
    }

    public String getName() {
        return this.name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getArtist() {
        return this.artist;
    }

    public void setArtist(String artist) {
        this.artist = artist;
    }

    public String getAuthor() {
        return this.author;
    }

    public void setAuthor(String author) {
        this.author = author;
    }

    public String getEditor() {
        return this.editor;
    }

    public void setEditor(String editor) {
        this.editor = editor;
    }

    public String[] getLyrics() {
        return this.lyrics;
    }

    public void setLyrics(String[] lyrics) {
        this.lyrics = lyrics;
    }

    public String[] getMusicalNotes() {
        return this.musicalNotes;
    }

    public void setMusicalNotes(String[] musicalNotes) {
        this.musicalNotes = musicalNotes;
    }

    public int getDuration() {
        return this.duration;
    }

    public void setDuration(int duration) {
        this.duration = duration;
    }

    public int getN() {
        return this.n;
    }

    public void setN(int n) {
        this.n = n;
    }

    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Nome da música: ").append(this.name).append("\n");
        sb.append("Artista: ").append(this.artist).append("\n");
        sb.append("Autor: ").append(this.author).append("\n");
        sb.append("Editor: ").append(this.editor).append("\n");
        sb.append("Duração: ").append(this.duration).append(" segundos\n");
        sb.append("Número de notas musicais: ").append(this.musicalNotes.length).append("\n");
        sb.append("Letra:\n");

        for (String linha : this.lyrics) {
            sb.append(linha).append("\n");
        }
        return sb.toString();
    }
}
