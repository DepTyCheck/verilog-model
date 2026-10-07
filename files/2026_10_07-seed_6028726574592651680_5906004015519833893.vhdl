-- Seed: 6028726574592651680,5906004015519833893

library ieee;
use ieee.std_logic_1164.all;

entity jjzcq is
  port (ucmkt : buffer std_logic_vector(2 to 0); jymq : linkage integer);
end jjzcq;

architecture bgtbyloc of jjzcq is
  
begin
  -- Multi-driven assignments
  ucmkt <= ucmkt;
end bgtbyloc;

entity k is
  port (stxptsg : out real_vector(1 downto 4));
end k;

library ieee;
use ieee.std_logic_1164.all;

architecture kzo of k is
  signal pwkilbhqx : integer;
  signal fvchhldown : integer;
  signal aktkmyqnsw : std_logic_vector(2 to 0);
  signal zpuowuzm : integer;
  signal utmjvq : std_logic_vector(2 to 0);
begin
  qvoozamr : entity work.jjzcq
    port map (ucmkt => utmjvq, jymq => zpuowuzm);
  hrxwajrc : entity work.jjzcq
    port map (ucmkt => aktkmyqnsw, jymq => fvchhldown);
  ydgqqggsv : entity work.jjzcq
    port map (ucmkt => utmjvq, jymq => pwkilbhqx);
  
  -- Single-driven assignments
  stxptsg <= stxptsg;
  
  -- Multi-driven assignments
  utmjvq <= (others => '0');
  utmjvq <= (others => '0');
  aktkmyqnsw <= utmjvq;
  aktkmyqnsw <= utmjvq;
end kzo;

entity nlyafmown is
  port (e : inout time_vector(2 to 3); rtmebztji : inout real; rer : out boolean);
end nlyafmown;

architecture zys of nlyafmown is
  signal ecsq : real_vector(1 downto 4);
  signal qkipmzzgnn : real_vector(1 downto 4);
begin
  yrzjev : entity work.k
    port map (stxptsg => qkipmzzgnn);
  rxct : entity work.k
    port map (stxptsg => ecsq);
  
  -- Single-driven assignments
  rer <= rer;
  e <= e;
end zys;

entity ogjzyva is
  port (eaqgorpxis : inout time_vector(0 downto 4); oqbv : out integer; gxf : in boolean_vector(2 to 3));
end ogjzyva;

library ieee;
use ieee.std_logic_1164.all;

architecture olucg of ogjzyva is
  signal cgtpisxfu : std_logic_vector(2 to 0);
begin
  yk : entity work.jjzcq
    port map (ucmkt => cgtpisxfu, jymq => oqbv);
end olucg;



-- Seed after: 1245962653457906200,5906004015519833893
