-- Seed: 16972583446318405072,2511821214772927453

library ieee;
use ieee.std_logic_1164.all;

entity xrngb is
  port (qmxks : inout std_logic);
end xrngb;

architecture kx of xrngb is
  
begin
  
end kx;

entity akd is
  port (hvwq : buffer real; wtsxeatevl : out boolean);
end akd;

library ieee;
use ieee.std_logic_1164.all;

architecture jlcxy of akd is
  signal pgtmfwxck : std_logic;
begin
  n : entity work.xrngb
    port map (qmxks => pgtmfwxck);
  
  -- Single-driven assignments
  wtsxeatevl <= TRUE;
  
  -- Multi-driven assignments
  pgtmfwxck <= 'X';
  pgtmfwxck <= 'H';
  pgtmfwxck <= pgtmfwxck;
  pgtmfwxck <= 'Z';
end jlcxy;



-- Seed after: 6898421769583944051,2511821214772927453
