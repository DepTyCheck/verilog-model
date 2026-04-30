-- Seed: 14429808518362875300,12368583144879391087

library ieee;
use ieee.std_logic_1164.all;

entity ypomjux is
  port (zzwjdx : in std_logic; btckifm : linkage boolean; svmhtgg : linkage real);
end ypomjux;



architecture jkkbbebtd of ypomjux is
  
begin
  
end jkkbbebtd;



entity tbq is
  port (chdpqbon : linkage time; weqeszmetb : out time; hkwepnks : out boolean);
end tbq;

library ieee;
use ieee.std_logic_1164.all;

architecture iacwy of tbq is
  signal vjrjppvjks : real;
  signal tagczn : std_logic;
  signal nowjkxwk : boolean;
  signal x : std_logic;
  signal esy : real;
  signal ufkauoe : std_logic;
begin
  sckjt : entity work.ypomjux
    port map (zzwjdx => ufkauoe, btckifm => hkwepnks, svmhtgg => esy);
  o : entity work.ypomjux
    port map (zzwjdx => x, btckifm => nowjkxwk, svmhtgg => esy);
  oppmirjwxq : entity work.ypomjux
    port map (zzwjdx => tagczn, btckifm => hkwepnks, svmhtgg => vjrjppvjks);
end iacwy;

library ieee;
use ieee.std_logic_1164.all;

entity otqt is
  port (yh : buffer std_logic; l : buffer std_logic);
end otqt;



architecture cvv of otqt is
  signal yuhxpa : boolean;
  signal gtdos : time;
begin
  gjwlz : entity work.tbq
    port map (chdpqbon => gtdos, weqeszmetb => gtdos, hkwepnks => yuhxpa);
end cvv;



entity zosyikv is
  port (jxiarcp : out real; axkzyztbkc : in time);
end zosyikv;

library ieee;
use ieee.std_logic_1164.all;

architecture zvklii of zosyikv is
  signal d : std_logic;
  signal gowej : real;
  signal majuqjsuy : std_logic;
  signal jdycywzoxg : boolean;
  signal k : time;
begin
  m : entity work.tbq
    port map (chdpqbon => axkzyztbkc, weqeszmetb => k, hkwepnks => jdycywzoxg);
  kuiwq : entity work.ypomjux
    port map (zzwjdx => majuqjsuy, btckifm => jdycywzoxg, svmhtgg => gowej);
  wbd : entity work.otqt
    port map (yh => majuqjsuy, l => d);
end zvklii;



-- Seed after: 4086728071454578905,12368583144879391087
