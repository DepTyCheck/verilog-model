-- Seed: 3289218081500055639,12269339630485015285

library ieee;
use ieee.std_logic_1164.all;

entity yb is
  port (rsmwc : linkage std_logic_vector(4 downto 0); gnlxmk : out integer; xmwed : in integer; kplkgcah : in integer);
end yb;

architecture peodgtrubf of yb is
  
begin
  -- Single-driven assignments
  gnlxmk <= 2#100#;
end peodgtrubf;

library ieee;
use ieee.std_logic_1164.all;

entity c is
  port (e : buffer std_logic; tm : linkage character; jiwra : linkage integer);
end c;

architecture vsobwp of c is
  
begin
  -- Multi-driven assignments
  e <= 'Z';
  e <= e;
  e <= e;
  e <= 'W';
end vsobwp;

entity vs is
  port (uvcynafzo : buffer integer_vector(2 to 0); vjdcillx : linkage time);
end vs;

library ieee;
use ieee.std_logic_1164.all;

architecture nv of vs is
  signal xdmdaeende : integer;
  signal rxu : integer;
  signal pzx : integer;
  signal pbelfwtw : integer;
  signal liiiqu : character;
  signal yntl : std_logic;
  signal qsxyqgwo : integer;
  signal xqsaev : integer;
  signal vsrjorzky : std_logic_vector(4 downto 0);
begin
  oyeilmddba : entity work.yb
    port map (rsmwc => vsrjorzky, gnlxmk => xqsaev, xmwed => qsxyqgwo, kplkgcah => xqsaev);
  vqeslmzq : entity work.c
    port map (e => yntl, tm => liiiqu, jiwra => pbelfwtw);
  vwvegd : entity work.yb
    port map (rsmwc => vsrjorzky, gnlxmk => pzx, xmwed => xqsaev, kplkgcah => xqsaev);
  t : entity work.yb
    port map (rsmwc => vsrjorzky, gnlxmk => rxu, xmwed => xdmdaeende, kplkgcah => pzx);
  
  -- Single-driven assignments
  qsxyqgwo <= xqsaev;
  uvcynafzo <= (others => 0);
  
  -- Multi-driven assignments
  vsrjorzky <= vsrjorzky;
  vsrjorzky <= "LXL-H";
  vsrjorzky <= ('L', 'Z', 'H', 'H', '-');
  yntl <= yntl;
end nv;

library ieee;
use ieee.std_logic_1164.all;

entity bxjk is
  port (icuazne : inout std_logic_vector(0 downto 4); zgekv : out bit_vector(3 downto 3); ry : inout string(4 downto 2));
end bxjk;

library ieee;
use ieee.std_logic_1164.all;

architecture bwflwgomw of bxjk is
  signal qvrzfpup : integer;
  signal xqttd : character;
  signal itsrll : std_logic;
  signal dtx : time;
  signal d : integer_vector(2 to 0);
begin
  myo : entity work.vs
    port map (uvcynafzo => d, vjdcillx => dtx);
  qcwrxy : entity work.c
    port map (e => itsrll, tm => xqttd, jiwra => qvrzfpup);
  
  -- Single-driven assignments
  zgekv <= zgekv;
end bwflwgomw;



-- Seed after: 7310831848702328691,12269339630485015285
