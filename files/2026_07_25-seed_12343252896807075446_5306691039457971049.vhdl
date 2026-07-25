-- Seed: 12343252896807075446,5306691039457971049

library ieee;
use ieee.std_logic_1164.all;

entity ftptk is
  port (scgmrf : out std_logic_vector(2 to 0));
end ftptk;

architecture vnjupijz of ftptk is
  
begin
  -- Multi-driven assignments
  scgmrf <= scgmrf;
end vnjupijz;

entity jmnooyrzg is
  port (rry : buffer time; atnkw : in time; lkqelao : buffer character);
end jmnooyrzg;

library ieee;
use ieee.std_logic_1164.all;

architecture jscnrsh of jmnooyrzg is
  signal meg : std_logic_vector(2 to 0);
  signal rkvwlq : std_logic_vector(2 to 0);
begin
  od : entity work.ftptk
    port map (scgmrf => rkvwlq);
  xxo : entity work.ftptk
    port map (scgmrf => meg);
  wvcplsv : entity work.ftptk
    port map (scgmrf => meg);
  
  -- Multi-driven assignments
  rkvwlq <= meg;
  rkvwlq <= (others => '0');
  meg <= (others => '0');
  rkvwlq <= meg;
end jscnrsh;

entity sqilmsw is
  port (vqsovyd : linkage boolean_vector(4 to 4); luvvumvpy : in bit);
end sqilmsw;

library ieee;
use ieee.std_logic_1164.all;

architecture grknzqt of sqilmsw is
  signal qdddstf : character;
  signal yiggdd : time;
  signal laktwbjifk : std_logic_vector(2 to 0);
  signal h : std_logic_vector(2 to 0);
begin
  g : entity work.ftptk
    port map (scgmrf => h);
  ylykrs : entity work.ftptk
    port map (scgmrf => laktwbjifk);
  vq : entity work.jmnooyrzg
    port map (rry => yiggdd, atnkw => yiggdd, lkqelao => qdddstf);
end grknzqt;



-- Seed after: 16687122780854705603,5306691039457971049
