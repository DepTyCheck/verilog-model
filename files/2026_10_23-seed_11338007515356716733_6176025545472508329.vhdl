-- Seed: 11338007515356716733,6176025545472508329

library ieee;
use ieee.std_logic_1164.all;

entity lsol is
  port (kdoxqz : in std_logic; reffmxlfm : in std_logic_vector(4 downto 4); tmhd : out std_logic_vector(2 to 0));
end lsol;

architecture yfzljovque of lsol is
  
begin
  -- Multi-driven assignments
  tmhd <= tmhd;
  tmhd <= "";
  tmhd <= tmhd;
end yfzljovque;

entity h is
  port (xvtzyjnwzm : buffer time_vector(3 to 4); fzocgu : inout severity_level);
end h;

library ieee;
use ieee.std_logic_1164.all;

architecture zgprvkktiq of h is
  signal irzcxqpout : std_logic_vector(2 to 0);
  signal t : std_logic;
  signal brnw : std_logic_vector(2 to 0);
  signal iicyjjahju : std_logic_vector(4 downto 4);
  signal eiegbeq : std_logic;
begin
  nxpons : entity work.lsol
    port map (kdoxqz => eiegbeq, reffmxlfm => iicyjjahju, tmhd => brnw);
  rf : entity work.lsol
    port map (kdoxqz => t, reffmxlfm => iicyjjahju, tmhd => irzcxqpout);
  
  -- Single-driven assignments
  xvtzyjnwzm <= (8#4_2_6# ns, 16#9_9_6_0_4# ps);
  
  -- Multi-driven assignments
  irzcxqpout <= brnw;
  eiegbeq <= '1';
  eiegbeq <= '-';
end zgprvkktiq;

library ieee;
use ieee.std_logic_1164.all;

entity ctxphqi is
  port (uttqybj : buffer std_logic; j : out real);
end ctxphqi;

library ieee;
use ieee.std_logic_1164.all;

architecture yejesx of ctxphqi is
  signal tvqzf : std_logic_vector(2 to 0);
  signal kquxfdb : std_logic_vector(4 downto 4);
  signal mhjhrpj : std_logic;
  signal znk : severity_level;
  signal umf : time_vector(3 to 4);
begin
  hvghfxhui : entity work.h
    port map (xvtzyjnwzm => umf, fzocgu => znk);
  bdwnfteyyj : entity work.lsol
    port map (kdoxqz => mhjhrpj, reffmxlfm => kquxfdb, tmhd => tvqzf);
  zxo : entity work.lsol
    port map (kdoxqz => uttqybj, reffmxlfm => kquxfdb, tmhd => tvqzf);
end yejesx;



-- Seed after: 7586271895044563068,6176025545472508329
