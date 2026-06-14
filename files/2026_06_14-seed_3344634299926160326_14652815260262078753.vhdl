-- Seed: 3344634299926160326,14652815260262078753

library ieee;
use ieee.std_logic_1164.all;

entity iac is
  port (qwt : out std_logic; bqbjlqpdim : buffer real; bwjfi : in std_logic_vector(0 to 0));
end iac;

architecture p of iac is
  
begin
  -- Multi-driven assignments
  qwt <= 'L';
end p;

entity mgrfh is
  port (lonyiy : buffer time; o : inout real; loa : in real);
end mgrfh;

library ieee;
use ieee.std_logic_1164.all;

architecture tijqi of mgrfh is
  signal tqfphlhxx : std_logic_vector(0 to 0);
  signal dvsqjhw : std_logic_vector(0 to 0);
  signal srhqxkn : real;
  signal ehdf : std_logic;
begin
  uwoi : entity work.iac
    port map (qwt => ehdf, bqbjlqpdim => srhqxkn, bwjfi => dvsqjhw);
  tbpqc : entity work.iac
    port map (qwt => ehdf, bqbjlqpdim => o, bwjfi => tqfphlhxx);
  
  -- Multi-driven assignments
  dvsqjhw <= (others => 'L');
  tqfphlhxx <= "X";
end tijqi;

entity digpuqx is
  port (pygrwofa : linkage real);
end digpuqx;

library ieee;
use ieee.std_logic_1164.all;

architecture qmbllts of digpuqx is
  signal eli : std_logic_vector(0 to 0);
  signal o : real;
  signal zn : std_logic_vector(0 to 0);
  signal w : real;
  signal kqyuffhz : std_logic;
begin
  ph : entity work.iac
    port map (qwt => kqyuffhz, bqbjlqpdim => w, bwjfi => zn);
  ojdbiazj : entity work.iac
    port map (qwt => kqyuffhz, bqbjlqpdim => o, bwjfi => eli);
  
  -- Multi-driven assignments
  zn <= (others => 'Z');
  zn <= (others => 'L');
  kqyuffhz <= 'U';
end qmbllts;



-- Seed after: 11606187532853850550,14652815260262078753
