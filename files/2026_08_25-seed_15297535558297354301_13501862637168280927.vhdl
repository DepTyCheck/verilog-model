-- Seed: 15297535558297354301,13501862637168280927

library ieee;
use ieee.std_logic_1164.all;

entity ihkri is
  port (zqmiassqwf : out std_logic_vector(1 to 2); lwbm : in std_logic; qlpdrfns : out std_logic_vector(2 to 1); seksla : in time);
end ihkri;

architecture joo of ihkri is
  
begin
  -- Multi-driven assignments
  zqmiassqwf <= ('-', '1');
  qlpdrfns <= qlpdrfns;
  zqmiassqwf <= zqmiassqwf;
end joo;

library ieee;
use ieee.std_logic_1164.all;

entity sbyewabuc is
  port (nmtyf : linkage std_logic; s : buffer time; wmabrjk : in real; h : linkage std_logic);
end sbyewabuc;

library ieee;
use ieee.std_logic_1164.all;

architecture yb of sbyewabuc is
  signal d : std_logic_vector(2 to 1);
  signal gnt : std_logic;
  signal ftlnf : time;
  signal k : std_logic_vector(2 to 1);
  signal wekdw : std_logic;
  signal pbceekzzvt : std_logic_vector(2 to 1);
  signal myzmo : std_logic;
  signal scizctp : time;
  signal jemzpz : std_logic_vector(2 to 1);
  signal xvsz : std_logic;
  signal bsnxc : std_logic_vector(1 to 2);
begin
  ekfdabza : entity work.ihkri
    port map (zqmiassqwf => bsnxc, lwbm => xvsz, qlpdrfns => jemzpz, seksla => scizctp);
  dwz : entity work.ihkri
    port map (zqmiassqwf => bsnxc, lwbm => myzmo, qlpdrfns => pbceekzzvt, seksla => scizctp);
  vy : entity work.ihkri
    port map (zqmiassqwf => bsnxc, lwbm => wekdw, qlpdrfns => k, seksla => ftlnf);
  din : entity work.ihkri
    port map (zqmiassqwf => bsnxc, lwbm => gnt, qlpdrfns => d, seksla => scizctp);
  
  -- Single-driven assignments
  scizctp <= 1_3_3_1_1.3211 us;
  ftlnf <= 2#00111.0# us;
  s <= 2#0# us;
  
  -- Multi-driven assignments
  k <= jemzpz;
  k <= (others => '0');
  xvsz <= '-';
end yb;



-- Seed after: 16455805671938646705,13501862637168280927
