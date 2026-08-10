-- Seed: 3240363904855093683,2338584220606314193

entity ddaok is
  port (oa : linkage bit_vector(3 downto 4); vqtwxi : inout integer; wutkvopkyv : linkage real);
end ddaok;

architecture gaztzqz of ddaok is
  
begin
  -- Single-driven assignments
  vqtwxi <= vqtwxi;
end gaztzqz;

library ieee;
use ieee.std_logic_1164.all;

entity wmqppswc is
  port (ukdwzxdocv : linkage bit; qiv : inout std_logic; zgdaqya : inout std_logic);
end wmqppswc;

architecture temkoqjoyw of wmqppswc is
  signal crug : real;
  signal hloag : integer;
  signal pfenbo : bit_vector(3 downto 4);
  signal nloyoxh : real;
  signal oavcgh : integer;
  signal yygepuwhom : bit_vector(3 downto 4);
  signal ikdyun : real;
  signal vbkmlor : integer;
  signal a : bit_vector(3 downto 4);
begin
  pj : entity work.ddaok
    port map (oa => a, vqtwxi => vbkmlor, wutkvopkyv => ikdyun);
  zrvj : entity work.ddaok
    port map (oa => yygepuwhom, vqtwxi => oavcgh, wutkvopkyv => nloyoxh);
  zfjq : entity work.ddaok
    port map (oa => pfenbo, vqtwxi => hloag, wutkvopkyv => crug);
  
  -- Multi-driven assignments
  zgdaqya <= 'H';
  zgdaqya <= 'H';
  zgdaqya <= '-';
  qiv <= '1';
end temkoqjoyw;

entity pkbedmkg is
  port (b : out boolean_vector(1 downto 2); uytzeov : buffer time);
end pkbedmkg;

architecture avgptdiw of pkbedmkg is
  signal kywbd : real;
  signal tddc : integer;
  signal ji : bit_vector(3 downto 4);
  signal lcwpeb : real;
  signal sh : integer;
  signal hut : bit_vector(3 downto 4);
  signal ea : real;
  signal ttrbompm : integer;
  signal sancennbkh : bit_vector(3 downto 4);
  signal ssmxf : real;
  signal sjkpmtjmpr : integer;
  signal xgptvfur : bit_vector(3 downto 4);
begin
  wdvwdi : entity work.ddaok
    port map (oa => xgptvfur, vqtwxi => sjkpmtjmpr, wutkvopkyv => ssmxf);
  ritloam : entity work.ddaok
    port map (oa => sancennbkh, vqtwxi => ttrbompm, wutkvopkyv => ea);
  rftbzdsbtj : entity work.ddaok
    port map (oa => hut, vqtwxi => sh, wutkvopkyv => lcwpeb);
  bny : entity work.ddaok
    port map (oa => ji, vqtwxi => tddc, wutkvopkyv => kywbd);
  
  -- Single-driven assignments
  uytzeov <= 8#2642.0# fs;
  b <= (others => TRUE);
end avgptdiw;



-- Seed after: 12473542830962371322,2338584220606314193
