-- Seed: 10298821405476065023,10557070023141912087

library ieee;
use ieee.std_logic_1164.all;

entity ibm is
  port (uxvharqs : linkage std_logic_vector(2 to 4); qdps : buffer bit);
end ibm;

architecture zaxya of ibm is
  
begin
  -- Single-driven assignments
  qdps <= '0';
end zaxya;

library ieee;
use ieee.std_logic_1164.all;

entity zilupk is
  port (fuzfoybi : linkage time; q : inout std_logic; ysnkeo : out time; umcyfxfp : inout std_logic);
end zilupk;

library ieee;
use ieee.std_logic_1164.all;

architecture jjcpw of zilupk is
  signal jblnl : bit;
  signal dfdf : std_logic_vector(2 to 4);
  signal bq : bit;
  signal ehbzibknx : std_logic_vector(2 to 4);
  signal v : bit;
  signal mg : std_logic_vector(2 to 4);
  signal qhjcbtx : bit;
  signal zohv : std_logic_vector(2 to 4);
begin
  e : entity work.ibm
    port map (uxvharqs => zohv, qdps => qhjcbtx);
  dijtjdsqh : entity work.ibm
    port map (uxvharqs => mg, qdps => v);
  fguukvph : entity work.ibm
    port map (uxvharqs => ehbzibknx, qdps => bq);
  zjxi : entity work.ibm
    port map (uxvharqs => dfdf, qdps => jblnl);
  
  -- Single-driven assignments
  ysnkeo <= 201.0 us;
end jjcpw;

entity h is
  port (nofzkxwow : inout integer);
end h;

library ieee;
use ieee.std_logic_1164.all;

architecture sjyury of h is
  signal yqbkc : bit;
  signal jl : bit;
  signal qjybck : std_logic_vector(2 to 4);
  signal cux : bit;
  signal gookge : std_logic_vector(2 to 4);
begin
  s : entity work.ibm
    port map (uxvharqs => gookge, qdps => cux);
  aefx : entity work.ibm
    port map (uxvharqs => qjybck, qdps => jl);
  rcuvkt : entity work.ibm
    port map (uxvharqs => gookge, qdps => yqbkc);
  
  -- Single-driven assignments
  nofzkxwow <= 2#1#;
  
  -- Multi-driven assignments
  gookge <= ('-', 'X', 'H');
end sjyury;

entity jfnbpdwn is
  port (zpwaznjmyw : linkage integer; gmjwx : in time);
end jfnbpdwn;

library ieee;
use ieee.std_logic_1164.all;

architecture deumyvt of jfnbpdwn is
  signal yfjnx : bit;
  signal uzwio : bit;
  signal gru : std_logic_vector(2 to 4);
  signal dk : std_logic;
  signal kjteisa : time;
  signal riodyrbthu : std_logic;
  signal ugpwkgez : time;
begin
  igitch : entity work.zilupk
    port map (fuzfoybi => ugpwkgez, q => riodyrbthu, ysnkeo => kjteisa, umcyfxfp => dk);
  lyqc : entity work.ibm
    port map (uxvharqs => gru, qdps => uzwio);
  wdmhkqiha : entity work.ibm
    port map (uxvharqs => gru, qdps => yfjnx);
  
  -- Multi-driven assignments
  riodyrbthu <= 'Z';
  dk <= '-';
  riodyrbthu <= '1';
end deumyvt;



-- Seed after: 13962739761803912518,10557070023141912087
