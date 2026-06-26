-- Seed: 4675572063266842615,12011142928354116943

library ieee;
use ieee.std_logic_1164.all;

entity due is
  port (joyr : inout integer; fardkn : linkage std_logic_vector(0 to 0); seukkbpbes : inout boolean_vector(1 downto 0));
end due;

architecture ols of due is
  
begin
  -- Single-driven assignments
  seukkbpbes <= (TRUE, TRUE);
  joyr <= 16#D_9#;
end ols;

entity kfddt is
  port (htjsngw : linkage real_vector(4 downto 2); nhnpajh : linkage time);
end kfddt;

library ieee;
use ieee.std_logic_1164.all;

architecture lu of kfddt is
  signal orpnm : boolean_vector(1 downto 0);
  signal esmr : integer;
  signal qhn : boolean_vector(1 downto 0);
  signal ypvjiuovg : std_logic_vector(0 to 0);
  signal tfs : integer;
begin
  ugltaf : entity work.due
    port map (joyr => tfs, fardkn => ypvjiuovg, seukkbpbes => qhn);
  fefnbcnt : entity work.due
    port map (joyr => esmr, fardkn => ypvjiuovg, seukkbpbes => orpnm);
  
  -- Multi-driven assignments
  ypvjiuovg <= (others => 'U');
  ypvjiuovg <= (others => 'Z');
  ypvjiuovg <= (others => 'L');
  ypvjiuovg <= "U";
end lu;

library ieee;
use ieee.std_logic_1164.all;

entity dw is
  port (qbku : in real; xgzsyi : in std_logic; yqbpmy : buffer real);
end dw;

architecture k of dw is
  
begin
  -- Single-driven assignments
  yqbpmy <= 8#74711.0622#;
end k;

entity m is
  port (elcbcu : out integer; lfssh : inout time; uqigu : in time_vector(1 downto 4); vhvxbsdjgw : buffer string(1 downto 4));
end m;

library ieee;
use ieee.std_logic_1164.all;

architecture bpzaobna of m is
  signal ehfzquszpj : real_vector(4 downto 2);
  signal slijmmhlqt : std_logic;
  signal ksfejduewf : real;
  signal eber : boolean_vector(1 downto 0);
  signal auccgmwvw : boolean_vector(1 downto 0);
  signal d : std_logic_vector(0 to 0);
  signal itxcfi : integer;
begin
  anioj : entity work.due
    port map (joyr => itxcfi, fardkn => d, seukkbpbes => auccgmwvw);
  dsoibvdld : entity work.due
    port map (joyr => elcbcu, fardkn => d, seukkbpbes => eber);
  gz : entity work.dw
    port map (qbku => ksfejduewf, xgzsyi => slijmmhlqt, yqbpmy => ksfejduewf);
  fivzlmg : entity work.kfddt
    port map (htjsngw => ehfzquszpj, nhnpajh => lfssh);
  
  -- Single-driven assignments
  vhvxbsdjgw <= (others => ' ');
  
  -- Multi-driven assignments
  slijmmhlqt <= 'Z';
end bpzaobna;



-- Seed after: 13804274464854544151,12011142928354116943
