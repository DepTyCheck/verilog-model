-- Seed: 12358409943748442053,9900838946149614537

entity aiixhng is
  port (b : inout time);
end aiixhng;

architecture kv of aiixhng is
  
begin
  -- Single-driven assignments
  b <= 16#BB8# us;
end kv;

entity p is
  port (qmbxpiy : in integer);
end p;

architecture dwsc of p is
  
begin
  
end dwsc;

library ieee;
use ieee.std_logic_1164.all;

entity k is
  port (jgwbnupi : inout integer; bvfqjq : linkage real; cp : out std_logic);
end k;

architecture pmbwbxmyi of k is
  signal kcygywwjgl : integer;
  signal brhotfl : time;
  signal fciqpfctyq : time;
  signal io : time;
begin
  ndo : entity work.aiixhng
    port map (b => io);
  ajsup : entity work.aiixhng
    port map (b => fciqpfctyq);
  umgrdt : entity work.aiixhng
    port map (b => brhotfl);
  qmmkhxyq : entity work.p
    port map (qmbxpiy => kcygywwjgl);
  
  -- Single-driven assignments
  kcygywwjgl <= jgwbnupi;
  jgwbnupi <= jgwbnupi;
  
  -- Multi-driven assignments
  cp <= 'L';
  cp <= 'U';
end pmbwbxmyi;



-- Seed after: 15346047005087987072,9900838946149614537
