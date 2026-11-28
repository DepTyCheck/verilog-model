-- Seed: 477427282893081398,15306146742274979323

library ieee;
use ieee.std_logic_1164.all;

entity ltcd is
  port (ffnm : inout time; l : buffer std_logic_vector(4 downto 2); dc : out character; xo : inout std_logic_vector(0 to 1));
end ltcd;

architecture w of ltcd is
  
begin
  -- Multi-driven assignments
  xo <= xo;
  xo <= xo;
  xo <= xo;
  xo <= ('-', 'H');
end w;

library ieee;
use ieee.std_logic_1164.all;

entity xjvuhzurxb is
  port (lbeuuyen : inout integer; hkmcjrwuef : linkage std_logic_vector(1 to 0); cdvstlcyc : inout bit_vector(2 downto 3); uoahigy : inout character);
end xjvuhzurxb;

library ieee;
use ieee.std_logic_1164.all;

architecture njzuc of xjvuhzurxb is
  signal nv : std_logic_vector(0 to 1);
  signal p : std_logic_vector(4 downto 2);
  signal rtkqtbhlnn : time;
begin
  deeq : entity work.ltcd
    port map (ffnm => rtkqtbhlnn, l => p, dc => uoahigy, xo => nv);
  
  -- Single-driven assignments
  cdvstlcyc <= cdvstlcyc;
  lbeuuyen <= 001;
  
  -- Multi-driven assignments
  nv <= nv;
  nv <= ('0', '1');
  p <= p;
end njzuc;

entity usohtmujmq is
  port (yya : inout time);
end usohtmujmq;

library ieee;
use ieee.std_logic_1164.all;

architecture wgzwtge of usohtmujmq is
  signal iutxaxj : character;
  signal nkehbe : std_logic_vector(0 to 1);
  signal eo : character;
  signal hoxm : std_logic_vector(4 downto 2);
  signal tmmtmi : time;
begin
  us : entity work.ltcd
    port map (ffnm => tmmtmi, l => hoxm, dc => eo, xo => nkehbe);
  wgvfyzuwhz : entity work.ltcd
    port map (ffnm => yya, l => hoxm, dc => iutxaxj, xo => nkehbe);
  
  -- Multi-driven assignments
  nkehbe <= ('W', '-');
  hoxm <= hoxm;
  hoxm <= hoxm;
  hoxm <= ('0', '-', '1');
end wgzwtge;

library ieee;
use ieee.std_logic_1164.all;

entity dqo is
  port (qyonx : buffer integer_vector(2 to 2); nsndqexny : linkage real; kiu : out std_logic_vector(0 downto 3));
end dqo;

library ieee;
use ieee.std_logic_1164.all;

architecture l of dqo is
  signal eq : std_logic_vector(0 to 1);
  signal melttx : character;
  signal c : time;
  signal nnsxkvpel : character;
  signal mpdzk : bit_vector(2 downto 3);
  signal nuukehrcg : std_logic_vector(1 to 0);
  signal la : integer;
  signal wcxgvy : std_logic_vector(0 to 1);
  signal k : character;
  signal pdz : std_logic_vector(4 downto 2);
  signal qtrjjttqjn : time;
begin
  zrmojrv : entity work.ltcd
    port map (ffnm => qtrjjttqjn, l => pdz, dc => k, xo => wcxgvy);
  vgyeahh : entity work.xjvuhzurxb
    port map (lbeuuyen => la, hkmcjrwuef => nuukehrcg, cdvstlcyc => mpdzk, uoahigy => nnsxkvpel);
  xroeqy : entity work.ltcd
    port map (ffnm => c, l => pdz, dc => melttx, xo => eq);
  
  -- Single-driven assignments
  qyonx <= qyonx;
  
  -- Multi-driven assignments
  nuukehrcg <= (others => '0');
end l;



-- Seed after: 10402548531062035159,15306146742274979323
