-- Seed: 4379487957338290367,10463297573877745897

library ieee;
use ieee.std_logic_1164.all;

entity gnutwedty is
  port (cuksqltkn : linkage std_logic_vector(1 to 3); nd : linkage integer_vector(4 downto 4); fn : inout real);
end gnutwedty;

architecture dbywxppfaf of gnutwedty is
  
begin
  
end dbywxppfaf;

entity e is
  port (ewoaazrukx : in integer);
end e;

library ieee;
use ieee.std_logic_1164.all;

architecture bqf of e is
  signal rtnilhldky : real;
  signal futks : integer_vector(4 downto 4);
  signal sxfhixc : real;
  signal lqebazt : integer_vector(4 downto 4);
  signal ndmti : real;
  signal xtwivmor : integer_vector(4 downto 4);
  signal vuyqvbt : std_logic_vector(1 to 3);
begin
  yqv : entity work.gnutwedty
    port map (cuksqltkn => vuyqvbt, nd => xtwivmor, fn => ndmti);
  avr : entity work.gnutwedty
    port map (cuksqltkn => vuyqvbt, nd => lqebazt, fn => sxfhixc);
  kzmxgwktw : entity work.gnutwedty
    port map (cuksqltkn => vuyqvbt, nd => futks, fn => rtnilhldky);
  
  -- Multi-driven assignments
  vuyqvbt <= ('L', 'L', 'X');
  vuyqvbt <= ('L', '1', 'U');
  vuyqvbt <= ('-', 'L', '1');
end bqf;

entity ygzr is
  port (nmkupkydbg : buffer time; spnn : buffer integer);
end ygzr;

library ieee;
use ieee.std_logic_1164.all;

architecture xylxemarm of ygzr is
  signal tueqqlzknw : real;
  signal wvewaqc : integer_vector(4 downto 4);
  signal ud : std_logic_vector(1 to 3);
  signal rqxhx : real;
  signal qfiunsgsgt : integer_vector(4 downto 4);
  signal wsnmvt : std_logic_vector(1 to 3);
begin
  ngyyt : entity work.e
    port map (ewoaazrukx => spnn);
  rcrcmk : entity work.gnutwedty
    port map (cuksqltkn => wsnmvt, nd => qfiunsgsgt, fn => rqxhx);
  as : entity work.gnutwedty
    port map (cuksqltkn => ud, nd => wvewaqc, fn => tueqqlzknw);
  
  -- Single-driven assignments
  spnn <= spnn;
  nmkupkydbg <= nmkupkydbg;
  
  -- Multi-driven assignments
  ud <= "ZXL";
end xylxemarm;



-- Seed after: 291200532290024222,10463297573877745897
