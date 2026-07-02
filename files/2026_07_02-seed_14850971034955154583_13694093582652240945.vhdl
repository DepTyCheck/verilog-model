-- Seed: 14850971034955154583,13694093582652240945

library ieee;
use ieee.std_logic_1164.all;

entity bjxaib is
  port (lvjy : inout std_logic; yytsinl : buffer integer; ucejgxsp : linkage std_logic_vector(3 to 4));
end bjxaib;

architecture ihgsdr of bjxaib is
  
begin
  -- Multi-driven assignments
  lvjy <= 'Z';
  lvjy <= 'L';
  lvjy <= 'Z';
  lvjy <= 'L';
end ihgsdr;

library ieee;
use ieee.std_logic_1164.all;

entity xfa is
  port (gvqghubinm : inout std_logic_vector(3 to 2));
end xfa;

library ieee;
use ieee.std_logic_1164.all;

architecture xhh of xfa is
  signal lvfeht : std_logic_vector(3 to 4);
  signal p : integer;
  signal ieab : std_logic;
  signal rel : std_logic_vector(3 to 4);
  signal vr : integer;
  signal pwusr : std_logic_vector(3 to 4);
  signal cxijhsfvk : integer;
  signal ckbzpyg : std_logic;
begin
  hakoryad : entity work.bjxaib
    port map (lvjy => ckbzpyg, yytsinl => cxijhsfvk, ucejgxsp => pwusr);
  uqkxexnefu : entity work.bjxaib
    port map (lvjy => ckbzpyg, yytsinl => vr, ucejgxsp => rel);
  fjwftioekm : entity work.bjxaib
    port map (lvjy => ieab, yytsinl => p, ucejgxsp => lvfeht);
end xhh;



-- Seed after: 15758471074100993347,13694093582652240945
