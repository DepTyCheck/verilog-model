-- Seed: 18232746700470311375,3687118713772291287

entity xho is
  port (wqkdnxdsof : linkage time; olgkcphiw : buffer time; sjfsp : inout integer; qan : out integer);
end xho;

architecture dznppxegf of xho is
  
begin
  -- Single-driven assignments
  sjfsp <= 3;
  qan <= 3144;
end dznppxegf;

library ieee;
use ieee.std_logic_1164.all;

entity lhogk is
  port (hahmlzbcnq : out std_logic; wovhqyygai : in bit_vector(2 downto 0));
end lhogk;

architecture evbw of lhogk is
  signal uguxuntxr : integer;
  signal yhl : integer;
  signal yqanfsfwzj : time;
  signal ohffe : time;
  signal pihgnon : integer;
  signal iihyon : integer;
  signal ysi : time;
  signal vlv : time;
  signal c : integer;
  signal craddqpb : integer;
  signal jove : time;
  signal jhkbzs : time;
begin
  u : entity work.xho
    port map (wqkdnxdsof => jhkbzs, olgkcphiw => jove, sjfsp => craddqpb, qan => c);
  rijnzniwhd : entity work.xho
    port map (wqkdnxdsof => vlv, olgkcphiw => ysi, sjfsp => iihyon, qan => pihgnon);
  z : entity work.xho
    port map (wqkdnxdsof => ohffe, olgkcphiw => yqanfsfwzj, sjfsp => yhl, qan => uguxuntxr);
  
  -- Multi-driven assignments
  hahmlzbcnq <= '-';
end evbw;

entity awcljtqo is
  port (oj : buffer integer);
end awcljtqo;

library ieee;
use ieee.std_logic_1164.all;

architecture lcfakhyktm of awcljtqo is
  signal gnl : bit_vector(2 downto 0);
  signal rfl : std_logic;
begin
  d : entity work.lhogk
    port map (hahmlzbcnq => rfl, wovhqyygai => gnl);
  
  -- Single-driven assignments
  oj <= 24;
  gnl <= ('1', '1', '0');
  
  -- Multi-driven assignments
  rfl <= '-';
  rfl <= '0';
end lcfakhyktm;

library ieee;
use ieee.std_logic_1164.all;

entity nsw is
  port (toolnrs : in integer; qhvlgxfk : in std_logic_vector(1 downto 0); kvyattfe : linkage std_logic_vector(2 to 0));
end nsw;

architecture nd of nsw is
  
begin
  
end nd;



-- Seed after: 9297659922218220697,3687118713772291287
