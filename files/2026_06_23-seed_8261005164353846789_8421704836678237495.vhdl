-- Seed: 8261005164353846789,8421704836678237495

library ieee;
use ieee.std_logic_1164.all;

entity df is
  port (ipb : in time; qrrmkjxun : in integer; zpqjxlnccu : inout boolean_vector(3 downto 3); nvvyzetnu : out std_logic_vector(1 downto 3));
end df;

architecture lqeshttly of df is
  
begin
  -- Multi-driven assignments
  nvvyzetnu <= "";
  nvvyzetnu <= "";
  nvvyzetnu <= (others => '0');
  nvvyzetnu <= (others => '0');
end lqeshttly;

entity lpdfcrla is
  port (yixyqpr : linkage integer);
end lpdfcrla;

library ieee;
use ieee.std_logic_1164.all;

architecture uzutmzshlx of lpdfcrla is
  signal jqfvvu : std_logic_vector(1 downto 3);
  signal nfudb : boolean_vector(3 downto 3);
  signal dxelgfmqbf : integer;
  signal yzznfc : std_logic_vector(1 downto 3);
  signal d : boolean_vector(3 downto 3);
  signal bjmz : integer;
  signal l : time;
begin
  xemg : entity work.df
    port map (ipb => l, qrrmkjxun => bjmz, zpqjxlnccu => d, nvvyzetnu => yzznfc);
  cfzhbbz : entity work.df
    port map (ipb => l, qrrmkjxun => dxelgfmqbf, zpqjxlnccu => nfudb, nvvyzetnu => jqfvvu);
  
  -- Single-driven assignments
  dxelgfmqbf <= 2#0_0_1#;
  bjmz <= 2#0010#;
  l <= 0 hr;
  
  -- Multi-driven assignments
  jqfvvu <= "";
end uzutmzshlx;

library ieee;
use ieee.std_logic_1164.all;

entity kyz is
  port (gsqdhro : in integer_vector(4 to 0); umff : inout boolean_vector(4 downto 2); u : in std_logic_vector(4 downto 0); ltfqgnb : in severity_level);
end kyz;

library ieee;
use ieee.std_logic_1164.all;

architecture gne of kyz is
  signal pngmbt : boolean_vector(3 downto 3);
  signal exknka : integer;
  signal oi : time;
  signal dq : boolean_vector(3 downto 3);
  signal xyvukatvaz : time;
  signal ocjc : std_logic_vector(1 downto 3);
  signal bkn : boolean_vector(3 downto 3);
  signal k : integer;
  signal ibwxtxufsm : time;
begin
  lwtswxa : entity work.df
    port map (ipb => ibwxtxufsm, qrrmkjxun => k, zpqjxlnccu => bkn, nvvyzetnu => ocjc);
  gphan : entity work.df
    port map (ipb => xyvukatvaz, qrrmkjxun => k, zpqjxlnccu => dq, nvvyzetnu => ocjc);
  i : entity work.df
    port map (ipb => oi, qrrmkjxun => exknka, zpqjxlnccu => pngmbt, nvvyzetnu => ocjc);
  
  -- Single-driven assignments
  exknka <= 2#10010#;
  k <= 1_1_1_4_1;
  xyvukatvaz <= 1 hr;
end gne;

library ieee;
use ieee.std_logic_1164.all;

entity zsm is
  port (lx : in std_logic; ib : linkage bit);
end zsm;

library ieee;
use ieee.std_logic_1164.all;

architecture fbvu of zsm is
  signal yunaxmry : std_logic_vector(1 downto 3);
  signal feivbqet : boolean_vector(3 downto 3);
  signal dmwwl : time;
  signal lhulj : severity_level;
  signal cwngokdq : std_logic_vector(4 downto 0);
  signal ademcf : boolean_vector(4 downto 2);
  signal p : integer_vector(4 to 0);
  signal bwsp : integer;
begin
  kbcxeogai : entity work.lpdfcrla
    port map (yixyqpr => bwsp);
  lxspfoic : entity work.kyz
    port map (gsqdhro => p, umff => ademcf, u => cwngokdq, ltfqgnb => lhulj);
  rforvu : entity work.df
    port map (ipb => dmwwl, qrrmkjxun => bwsp, zpqjxlnccu => feivbqet, nvvyzetnu => yunaxmry);
  
  -- Single-driven assignments
  p <= (others => 0);
  lhulj <= WARNING;
  
  -- Multi-driven assignments
  cwngokdq <= ('X', 'X', 'H', '-', 'X');
  yunaxmry <= "";
  cwngokdq <= ('-', 'U', '-', 'L', '0');
  cwngokdq <= "0W-X0";
end fbvu;



-- Seed after: 14334231709818259608,8421704836678237495
