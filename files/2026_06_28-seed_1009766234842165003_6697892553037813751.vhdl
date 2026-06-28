-- Seed: 1009766234842165003,6697892553037813751

library ieee;
use ieee.std_logic_1164.all;

entity hequupftj is
  port (a : buffer std_logic; wrelg : in integer; uoka : buffer integer; kqjbhshqe : in character);
end hequupftj;

architecture wjovrpnhlu of hequupftj is
  
begin
  -- Single-driven assignments
  uoka <= 2#10101#;
  
  -- Multi-driven assignments
  a <= 'L';
  a <= 'H';
  a <= 'U';
end wjovrpnhlu;

entity itqjkadt is
  port (wgwr : inout bit; nrtvl : out character; r : buffer time);
end itqjkadt;

library ieee;
use ieee.std_logic_1164.all;

architecture khspft of itqjkadt is
  signal zmchdlagzj : character;
  signal hdxecxdh : integer;
  signal dy : integer;
  signal awbkbn : integer;
  signal qk : std_logic;
begin
  myfp : entity work.hequupftj
    port map (a => qk, wrelg => awbkbn, uoka => awbkbn, kqjbhshqe => nrtvl);
  qtcyjmpjoh : entity work.hequupftj
    port map (a => qk, wrelg => dy, uoka => hdxecxdh, kqjbhshqe => zmchdlagzj);
  
  -- Single-driven assignments
  nrtvl <= 'k';
  r <= 2 min;
  
  -- Multi-driven assignments
  qk <= 'U';
  qk <= 'W';
  qk <= 'H';
end khspft;

library ieee;
use ieee.std_logic_1164.all;

entity f is
  port (otvxkawki : inout boolean_vector(2 to 0); d : inout std_logic_vector(2 to 1));
end f;

library ieee;
use ieee.std_logic_1164.all;

architecture nmyuvz of f is
  signal mpzphojh : time;
  signal kjzebeytdo : bit;
  signal tqtrwmb : character;
  signal rgrgrwaia : integer;
  signal mnofzaw : std_logic;
begin
  hhi : entity work.hequupftj
    port map (a => mnofzaw, wrelg => rgrgrwaia, uoka => rgrgrwaia, kqjbhshqe => tqtrwmb);
  mj : entity work.itqjkadt
    port map (wgwr => kjzebeytdo, nrtvl => tqtrwmb, r => mpzphojh);
  
  -- Single-driven assignments
  otvxkawki <= (others => TRUE);
  
  -- Multi-driven assignments
  mnofzaw <= '-';
  mnofzaw <= 'W';
  d <= "";
end nmyuvz;



-- Seed after: 17828628466584616292,6697892553037813751
