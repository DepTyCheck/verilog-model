-- Seed: 2266608928657045105,11127274767545411571

library ieee;
use ieee.std_logic_1164.all;

entity vd is
  port (daworerchb : in integer; jqijrf : in std_logic_vector(0 to 0); sxmrzvebx : inout time; plczdmwvd : buffer std_logic);
end vd;

architecture sqru of vd is
  
begin
  -- Single-driven assignments
  sxmrzvebx <= sxmrzvebx;
  
  -- Multi-driven assignments
  plczdmwvd <= 'H';
end sqru;

library ieee;
use ieee.std_logic_1164.all;

entity lbvv is
  port (vneqe : inout time; mdimoapn : linkage string(5 downto 4); aboh : inout real; mo : out std_logic_vector(3 downto 3));
end lbvv;

library ieee;
use ieee.std_logic_1164.all;

architecture pmzjcexk of lbvv is
  signal frtspetfh : std_logic;
  signal iwgxw : std_logic_vector(0 to 0);
  signal v : integer;
  signal xlaly : std_logic;
  signal rrwrigjxr : time;
  signal rhkltypw : integer;
  signal laabut : std_logic;
  signal moun : time;
  signal bnqu : std_logic_vector(0 to 0);
  signal duca : std_logic;
  signal spcvb : time;
  signal omjt : std_logic_vector(0 to 0);
  signal zmfy : integer;
begin
  pt : entity work.vd
    port map (daworerchb => zmfy, jqijrf => omjt, sxmrzvebx => spcvb, plczdmwvd => duca);
  g : entity work.vd
    port map (daworerchb => zmfy, jqijrf => bnqu, sxmrzvebx => moun, plczdmwvd => laabut);
  tgxlkgs : entity work.vd
    port map (daworerchb => rhkltypw, jqijrf => bnqu, sxmrzvebx => rrwrigjxr, plczdmwvd => xlaly);
  xhce : entity work.vd
    port map (daworerchb => v, jqijrf => iwgxw, sxmrzvebx => vneqe, plczdmwvd => frtspetfh);
  
  -- Single-driven assignments
  zmfy <= 2#1_0_1_1#;
  v <= zmfy;
  rhkltypw <= 0;
  aboh <= aboh;
  
  -- Multi-driven assignments
  omjt <= (others => 'X');
  duca <= '-';
end pmzjcexk;



-- Seed after: 9491046077669576129,11127274767545411571
