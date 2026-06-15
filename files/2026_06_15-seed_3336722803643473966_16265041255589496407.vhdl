-- Seed: 3336722803643473966,16265041255589496407



entity tudfihhq is
  port (hptxbh : out character; kcth : in time_vector(3 downto 3));
end tudfihhq;



architecture dboo of tudfihhq is
  
begin
  
end dboo;



entity eey is
  port (a : inout bit; hk : linkage boolean_vector(1 to 4));
end eey;



architecture bhybkmqi of eey is
  signal hletuczfhc : time_vector(3 downto 3);
  signal ni : character;
begin
  vcojri : entity work.tudfihhq
    port map (hptxbh => ni, kcth => hletuczfhc);
end bhybkmqi;



entity oyiwtl is
  port (dnidqlt : out integer);
end oyiwtl;



architecture dm of oyiwtl is
  signal uqpaetjb : boolean_vector(1 to 4);
  signal e : bit;
begin
  mduniqax : entity work.eey
    port map (a => e, hk => uqpaetjb);
end dm;

library ieee;
use ieee.std_logic_1164.all;

entity mjrg is
  port (spnvfdnrl : inout time_vector(4 to 0); trghppws : in std_logic_vector(3 downto 0));
end mjrg;



architecture thlep of mjrg is
  signal mjoic : time_vector(3 downto 3);
  signal l : character;
  signal jairruse : integer;
begin
  zexkd : entity work.oyiwtl
    port map (dnidqlt => jairruse);
  r : entity work.tudfihhq
    port map (hptxbh => l, kcth => mjoic);
end thlep;



-- Seed after: 2842464576053650777,16265041255589496407
