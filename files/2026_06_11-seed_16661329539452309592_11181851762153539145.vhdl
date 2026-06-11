-- Seed: 16661329539452309592,11181851762153539145

library ieee;
use ieee.std_logic_1164.all;

entity cdcezm is
  port (kpv : buffer std_logic_vector(1 downto 4); gwqmdsp : out real);
end cdcezm;



architecture tjvgugnc of cdcezm is
  
begin
  
end tjvgugnc;



entity bhobr is
  port (gcug : in time; faftixeqf : in real_vector(3 to 1));
end bhobr;

library ieee;
use ieee.std_logic_1164.all;

architecture mvmy of bhobr is
  signal mywaf : real;
  signal b : std_logic_vector(1 downto 4);
  signal fsie : real;
  signal a : std_logic_vector(1 downto 4);
begin
  r : entity work.cdcezm
    port map (kpv => a, gwqmdsp => fsie);
  prdj : entity work.cdcezm
    port map (kpv => b, gwqmdsp => mywaf);
end mvmy;



entity e is
  port (kiddvgkiz : out time_vector(3 to 2));
end e;

library ieee;
use ieee.std_logic_1164.all;

architecture htilsega of e is
  signal cmfrse : real_vector(3 to 1);
  signal bclorcepi : time;
  signal iikcmboxgz : real_vector(3 to 1);
  signal kcigkxrr : time;
  signal uzyueivuty : real;
  signal alzcwl : std_logic_vector(1 downto 4);
begin
  drxuc : entity work.cdcezm
    port map (kpv => alzcwl, gwqmdsp => uzyueivuty);
  gmfb : entity work.bhobr
    port map (gcug => kcigkxrr, faftixeqf => iikcmboxgz);
  ogdohbt : entity work.bhobr
    port map (gcug => bclorcepi, faftixeqf => cmfrse);
end htilsega;



-- Seed after: 1425707772946911824,11181851762153539145
