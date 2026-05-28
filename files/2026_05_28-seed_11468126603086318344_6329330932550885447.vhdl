-- Seed: 11468126603086318344,6329330932550885447



entity i is
  port (vds : linkage boolean_vector(0 to 4); icsemiu : inout time);
end i;



architecture xshsfpv of i is
  
begin
  
end xshsfpv;



entity bvcmnp is
  port (nekizjvg : buffer bit; cc : out integer);
end bvcmnp;



architecture mr of bvcmnp is
  signal qgmyokp : time;
  signal jovoq : time;
  signal qebcdcvz : boolean_vector(0 to 4);
begin
  yqamouhsa : entity work.i
    port map (vds => qebcdcvz, icsemiu => jovoq);
  yyhfsl : entity work.i
    port map (vds => qebcdcvz, icsemiu => qgmyokp);
end mr;

library ieee;
use ieee.std_logic_1164.all;

entity f is
  port (qujdkl : buffer bit; edqsaxmpz : inout std_logic_vector(4 to 3); oeijfwckf : in integer_vector(4 downto 4));
end f;



architecture qhihfimkje of f is
  signal xdqtmpnt : integer;
  signal bgv : bit;
  signal jo : time;
  signal plp : boolean_vector(0 to 4);
begin
  jyel : entity work.i
    port map (vds => plp, icsemiu => jo);
  deh : entity work.bvcmnp
    port map (nekizjvg => bgv, cc => xdqtmpnt);
end qhihfimkje;



-- Seed after: 14600658108649020584,6329330932550885447
