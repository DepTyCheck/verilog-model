-- Seed: 12154497762033104911,13613332369802491303

library ieee;
use ieee.std_logic_1164.all;

entity zgjlefngx is
  port (h : linkage integer; ayq : linkage integer; odrqmzgw : buffer std_logic; mxnlzwavex : inout time_vector(3 downto 2));
end zgjlefngx;

architecture ebb of zgjlefngx is
  
begin
  -- Single-driven assignments
  mxnlzwavex <= (16#D_B_A.0# ps, 8#6# ns);
  
  -- Multi-driven assignments
  odrqmzgw <= 'X';
  odrqmzgw <= odrqmzgw;
  odrqmzgw <= odrqmzgw;
  odrqmzgw <= odrqmzgw;
end ebb;

library ieee;
use ieee.std_logic_1164.all;

entity pdr is
  port (ifedruqhp : in boolean; meonvtti : linkage std_logic_vector(0 downto 4); mr : inout boolean_vector(4 downto 0));
end pdr;

library ieee;
use ieee.std_logic_1164.all;

architecture uobzwa of pdr is
  signal vhw : time_vector(3 downto 2);
  signal ekhy : std_logic;
  signal tly : integer;
  signal ckp : integer;
  signal wbifyj : time_vector(3 downto 2);
  signal yqga : std_logic;
  signal r : integer;
  signal zdiftz : integer;
begin
  nzluvnlms : entity work.zgjlefngx
    port map (h => zdiftz, ayq => r, odrqmzgw => yqga, mxnlzwavex => wbifyj);
  cf : entity work.zgjlefngx
    port map (h => ckp, ayq => tly, odrqmzgw => ekhy, mxnlzwavex => vhw);
  
  -- Single-driven assignments
  mr <= (TRUE, TRUE, TRUE, TRUE, TRUE);
  
  -- Multi-driven assignments
  ekhy <= yqga;
end uobzwa;

entity jic is
  port (obpgf : inout string(2 downto 3); rsygol : out integer);
end jic;

library ieee;
use ieee.std_logic_1164.all;

architecture rjimouy of jic is
  signal fxgqje : time_vector(3 downto 2);
  signal sqn : integer;
  signal zuktht : time_vector(3 downto 2);
  signal qnylmfzltq : std_logic;
  signal vekjm : integer;
  signal qceqav : integer;
begin
  zhkcibsw : entity work.zgjlefngx
    port map (h => qceqav, ayq => vekjm, odrqmzgw => qnylmfzltq, mxnlzwavex => zuktht);
  pkebhsao : entity work.zgjlefngx
    port map (h => sqn, ayq => rsygol, odrqmzgw => qnylmfzltq, mxnlzwavex => fxgqje);
  
  -- Single-driven assignments
  obpgf <= obpgf;
  
  -- Multi-driven assignments
  qnylmfzltq <= '-';
  qnylmfzltq <= 'L';
  qnylmfzltq <= qnylmfzltq;
  qnylmfzltq <= '-';
end rjimouy;



-- Seed after: 16276701581984662290,13613332369802491303
