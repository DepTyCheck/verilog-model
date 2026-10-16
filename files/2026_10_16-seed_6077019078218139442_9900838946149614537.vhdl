-- Seed: 6077019078218139442,9900838946149614537

library ieee;
use ieee.std_logic_1164.all;

entity qtgff is
  port (yjz : out time; sw : buffer std_logic_vector(0 to 3));
end qtgff;

architecture jkkgeobi of qtgff is
  
begin
  -- Single-driven assignments
  yjz <= yjz;
  
  -- Multi-driven assignments
  sw <= sw;
end jkkgeobi;

entity avpvy is
  port (twtjrevp : in real);
end avpvy;

library ieee;
use ieee.std_logic_1164.all;

architecture jqyfuq of avpvy is
  signal euvwiqfcs : time;
  signal ehuamraa : time;
  signal razkwe : std_logic_vector(0 to 3);
  signal veeeabl : time;
  signal yjkb : std_logic_vector(0 to 3);
  signal bzzwxt : time;
begin
  tu : entity work.qtgff
    port map (yjz => bzzwxt, sw => yjkb);
  sxkqdv : entity work.qtgff
    port map (yjz => veeeabl, sw => razkwe);
  kgufgexlx : entity work.qtgff
    port map (yjz => ehuamraa, sw => razkwe);
  xslbfsmidz : entity work.qtgff
    port map (yjz => euvwiqfcs, sw => razkwe);
  
  -- Multi-driven assignments
  yjkb <= "XLZL";
  yjkb <= ('1', 'X', 'W', 'H');
  razkwe <= razkwe;
  razkwe <= "11W0";
end jqyfuq;



-- Seed after: 9987674809976750530,9900838946149614537
