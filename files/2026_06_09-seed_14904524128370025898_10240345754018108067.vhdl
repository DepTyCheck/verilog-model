-- Seed: 14904524128370025898,10240345754018108067

library ieee;
use ieee.std_logic_1164.all;

entity ud is
  port (dffnnzblr : in integer_vector(1 to 2); zclze : buffer std_logic_vector(4 downto 3); nbbjxqcsfo : linkage boolean_vector(4 to 3));
end ud;



architecture egijtdecrd of ud is
  
begin
  
end egijtdecrd;



entity cym is
  port (gtbv : linkage bit_vector(0 downto 0); tm : buffer integer; bxiicaodvw : inout time);
end cym;



architecture lqlez of cym is
  
begin
  
end lqlez;

library ieee;
use ieee.std_logic_1164.all;

entity zpdwfxod is
  port (nehsrh : out std_logic_vector(1 to 2); thejcjdtd : in time);
end zpdwfxod;



architecture xmbldcyl of zpdwfxod is
  signal pyy : integer_vector(1 to 2);
  signal db : integer_vector(1 to 2);
  signal c : boolean_vector(4 to 3);
  signal elgfsav : integer_vector(1 to 2);
begin
  awakorujo : entity work.ud
    port map (dffnnzblr => elgfsav, zclze => nehsrh, nbbjxqcsfo => c);
  ykgjzzcjug : entity work.ud
    port map (dffnnzblr => db, zclze => nehsrh, nbbjxqcsfo => c);
  adsmsf : entity work.ud
    port map (dffnnzblr => pyy, zclze => nehsrh, nbbjxqcsfo => c);
end xmbldcyl;



entity ieoivvembp is
  port (hqwv : out integer);
end ieoivvembp;



architecture zhi of ieoivvembp is
  signal bcsrr : time;
  signal v : bit_vector(0 downto 0);
begin
  zptxwy : entity work.cym
    port map (gtbv => v, tm => hqwv, bxiicaodvw => bcsrr);
end zhi;



-- Seed after: 13245726293696258688,10240345754018108067
