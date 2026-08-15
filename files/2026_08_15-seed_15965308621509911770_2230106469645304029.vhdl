-- Seed: 15965308621509911770,2230106469645304029

library ieee;
use ieee.std_logic_1164.all;

entity qbmhkkfjj is
  port (d : buffer time; l : in time; trtyodi : out std_logic_vector(2 downto 3); yqlxv : out real);
end qbmhkkfjj;

architecture ihhhbxeh of qbmhkkfjj is
  
begin
  -- Single-driven assignments
  d <= l;
  yqlxv <= 16#8_B.90C#;
end ihhhbxeh;

entity qiaky is
  port (nmybprydd : in integer_vector(0 to 1));
end qiaky;

architecture clgvfwdlze of qiaky is
  
begin
  
end clgvfwdlze;

entity pgc is
  port (yylitucr : in boolean);
end pgc;

library ieee;
use ieee.std_logic_1164.all;

architecture zts of pgc is
  signal ng : real;
  signal bbblzdg : integer_vector(0 to 1);
  signal ykpmahmv : real;
  signal lfc : std_logic_vector(2 downto 3);
  signal llrvh : time;
  signal krpj : time;
begin
  k : entity work.qbmhkkfjj
    port map (d => krpj, l => llrvh, trtyodi => lfc, yqlxv => ykpmahmv);
  iytphx : entity work.qiaky
    port map (nmybprydd => bbblzdg);
  evoxzeqhuf : entity work.qbmhkkfjj
    port map (d => llrvh, l => llrvh, trtyodi => lfc, yqlxv => ng);
  
  -- Single-driven assignments
  bbblzdg <= bbblzdg;
end zts;



-- Seed after: 10473673777701146244,2230106469645304029
