-- Seed: 7727115747633752060,14641901754878719179

entity pdm is
  port (znwfwz : buffer time; pwqqmzewoi : out time_vector(0 downto 2));
end pdm;

architecture xijkzndh of pdm is
  
begin
  
end xijkzndh;

entity mvtr is
  port (hgz : out integer);
end mvtr;

architecture dpm of mvtr is
  signal pxarswp : time_vector(0 downto 2);
  signal o : time;
  signal qduo : time_vector(0 downto 2);
  signal itaoz : time;
  signal bigixxxm : time_vector(0 downto 2);
  signal liig : time;
  signal s : time_vector(0 downto 2);
  signal lvp : time;
begin
  idbnaoic : entity work.pdm
    port map (znwfwz => lvp, pwqqmzewoi => s);
  ztxwhkosnw : entity work.pdm
    port map (znwfwz => liig, pwqqmzewoi => bigixxxm);
  fmazku : entity work.pdm
    port map (znwfwz => itaoz, pwqqmzewoi => qduo);
  bppb : entity work.pdm
    port map (znwfwz => o, pwqqmzewoi => pxarswp);
end dpm;

entity eed is
  port (gnxvok : linkage integer);
end eed;

architecture wvgsxv of eed is
  signal nx : integer;
  signal fmjpbzgql : integer;
  signal ybudyblb : integer;
  signal q : time_vector(0 downto 2);
  signal nzz : time;
begin
  vunzeuwa : entity work.pdm
    port map (znwfwz => nzz, pwqqmzewoi => q);
  wqdhke : entity work.mvtr
    port map (hgz => ybudyblb);
  kuf : entity work.mvtr
    port map (hgz => fmjpbzgql);
  wuk : entity work.mvtr
    port map (hgz => nx);
end wvgsxv;



-- Seed after: 7673261190943637718,14641901754878719179
