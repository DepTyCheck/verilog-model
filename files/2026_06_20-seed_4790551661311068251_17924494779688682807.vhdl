-- Seed: 4790551661311068251,17924494779688682807

library ieee;
use ieee.std_logic_1164.all;

entity amjyylj is
  port (vpvwenru : inout std_logic_vector(3 to 1); txfbsgyqj : linkage std_logic_vector(2 to 2); zdcllvdb : linkage integer_vector(2 to 0));
end amjyylj;

architecture mnefxsgb of amjyylj is
  
begin
  -- Multi-driven assignments
  vpvwenru <= (others => '0');
  vpvwenru <= "";
  vpvwenru <= "";
end mnefxsgb;

entity qfjn is
  port (jvjzodr : out integer; sbzh : in integer);
end qfjn;

library ieee;
use ieee.std_logic_1164.all;

architecture lxodyktolc of qfjn is
  signal hfukhuczq : integer_vector(2 to 0);
  signal seuxoglzo : std_logic_vector(2 to 2);
  signal lab : integer_vector(2 to 0);
  signal oqhdwpmo : std_logic_vector(3 to 1);
  signal ziyjiid : integer_vector(2 to 0);
  signal qflgxl : std_logic_vector(2 to 2);
  signal a : std_logic_vector(3 to 1);
  signal yxa : integer_vector(2 to 0);
  signal phiw : std_logic_vector(2 to 2);
  signal r : std_logic_vector(3 to 1);
begin
  afbbvjpo : entity work.amjyylj
    port map (vpvwenru => r, txfbsgyqj => phiw, zdcllvdb => yxa);
  qrrfia : entity work.amjyylj
    port map (vpvwenru => a, txfbsgyqj => qflgxl, zdcllvdb => ziyjiid);
  jcr : entity work.amjyylj
    port map (vpvwenru => oqhdwpmo, txfbsgyqj => phiw, zdcllvdb => lab);
  rhhyvnob : entity work.amjyylj
    port map (vpvwenru => oqhdwpmo, txfbsgyqj => seuxoglzo, zdcllvdb => hfukhuczq);
  
  -- Single-driven assignments
  jvjzodr <= 16#5_C#;
end lxodyktolc;

library ieee;
use ieee.std_logic_1164.all;

entity qwmaduztwh is
  port (h : inout integer; zna : linkage std_logic_vector(4 to 0); fkzjdh : in bit_vector(3 to 1); hnbf : inout bit);
end qwmaduztwh;

library ieee;
use ieee.std_logic_1164.all;

architecture jxrvgin of qwmaduztwh is
  signal bokwmltucf : integer_vector(2 to 0);
  signal xdwdlvr : std_logic_vector(3 to 1);
  signal aghzwokjev : integer;
  signal rwebqerrnq : integer;
  signal dknqi : integer_vector(2 to 0);
  signal wcxsabnti : std_logic_vector(3 to 1);
  signal wwkilgnq : integer_vector(2 to 0);
  signal uqirgqem : std_logic_vector(2 to 2);
  signal cbaksr : std_logic_vector(3 to 1);
begin
  kive : entity work.amjyylj
    port map (vpvwenru => cbaksr, txfbsgyqj => uqirgqem, zdcllvdb => wwkilgnq);
  esidsv : entity work.amjyylj
    port map (vpvwenru => wcxsabnti, txfbsgyqj => uqirgqem, zdcllvdb => dknqi);
  drpwwrcwpa : entity work.qfjn
    port map (jvjzodr => rwebqerrnq, sbzh => aghzwokjev);
  zczopavs : entity work.amjyylj
    port map (vpvwenru => xdwdlvr, txfbsgyqj => uqirgqem, zdcllvdb => bokwmltucf);
  
  -- Single-driven assignments
  hnbf <= '1';
  aghzwokjev <= 2041;
  h <= 16#9#;
end jxrvgin;



-- Seed after: 11096083684615232282,17924494779688682807
