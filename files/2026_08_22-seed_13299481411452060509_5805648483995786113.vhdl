-- Seed: 13299481411452060509,5805648483995786113

library ieee;
use ieee.std_logic_1164.all;

entity fewgoh is
  port (hqgixhcp : in real; tjbmtwi : inout std_logic; gobknlklo : out boolean_vector(1 downto 4));
end fewgoh;

architecture e of fewgoh is
  
begin
  -- Single-driven assignments
  gobknlklo <= (others => TRUE);
  
  -- Multi-driven assignments
  tjbmtwi <= 'L';
  tjbmtwi <= tjbmtwi;
  tjbmtwi <= tjbmtwi;
end e;

entity iy is
  port (t : inout real; ejphioct : inout real);
end iy;

library ieee;
use ieee.std_logic_1164.all;

architecture lcoik of iy is
  signal s : boolean_vector(1 downto 4);
  signal peedbxwc : boolean_vector(1 downto 4);
  signal ckimjist : std_logic;
  signal oyror : boolean_vector(1 downto 4);
  signal fumrnk : std_logic;
  signal cpvesgqzp : real;
begin
  hlvzspnio : entity work.fewgoh
    port map (hqgixhcp => cpvesgqzp, tjbmtwi => fumrnk, gobknlklo => oyror);
  bfw : entity work.fewgoh
    port map (hqgixhcp => ejphioct, tjbmtwi => ckimjist, gobknlklo => peedbxwc);
  k : entity work.fewgoh
    port map (hqgixhcp => ejphioct, tjbmtwi => fumrnk, gobknlklo => s);
  
  -- Single-driven assignments
  ejphioct <= ejphioct;
  
  -- Multi-driven assignments
  fumrnk <= fumrnk;
  ckimjist <= fumrnk;
  ckimjist <= fumrnk;
  fumrnk <= fumrnk;
end lcoik;

library ieee;
use ieee.std_logic_1164.all;

entity xvzdkwaez is
  port (pujfeqe : linkage std_logic_vector(3 downto 4));
end xvzdkwaez;

library ieee;
use ieee.std_logic_1164.all;

architecture jttph of xvzdkwaez is
  signal nb : real;
  signal so : boolean_vector(1 downto 4);
  signal ryxha : std_logic;
  signal nbkffwjri : real;
  signal qu : boolean_vector(1 downto 4);
  signal x : std_logic;
  signal sc : real;
begin
  ky : entity work.fewgoh
    port map (hqgixhcp => sc, tjbmtwi => x, gobknlklo => qu);
  criv : entity work.fewgoh
    port map (hqgixhcp => nbkffwjri, tjbmtwi => ryxha, gobknlklo => so);
  cfdqdmxnwj : entity work.iy
    port map (t => sc, ejphioct => nb);
  
  -- Single-driven assignments
  nbkffwjri <= 2#1_1_0_1_0.0#;
  
  -- Multi-driven assignments
  x <= '-';
end jttph;



-- Seed after: 18288857568718785512,5805648483995786113
