-- Seed: 1383747710454934711,11481034001933599325

entity t is
  port (zfaqyy : in bit_vector(0 downto 4));
end t;

architecture uobd of t is
  
begin
  
end uobd;

library ieee;
use ieee.std_logic_1164.all;

entity ttbgphhbq is
  port (hgttwdmad : in std_logic_vector(3 to 1); ktb : in real_vector(3 to 0); olicheysqw : buffer std_logic);
end ttbgphhbq;

architecture dwizwxnaa of ttbgphhbq is
  signal gwdajkvxe : bit_vector(0 downto 4);
begin
  l : entity work.t
    port map (zfaqyy => gwdajkvxe);
  irzvbevtd : entity work.t
    port map (zfaqyy => gwdajkvxe);
  hrhllodmpo : entity work.t
    port map (zfaqyy => gwdajkvxe);
  
  -- Multi-driven assignments
  olicheysqw <= olicheysqw;
  olicheysqw <= olicheysqw;
  olicheysqw <= 'Z';
  olicheysqw <= olicheysqw;
end dwizwxnaa;

entity rsp is
  port (ndsoj : in integer; ppfahypzl : in time; gqjcjqwn : out real);
end rsp;

library ieee;
use ieee.std_logic_1164.all;

architecture yw of rsp is
  signal sjhb : std_logic;
  signal lauzwar : real_vector(3 to 0);
  signal n : std_logic_vector(3 to 1);
  signal jarclfdxke : bit_vector(0 downto 4);
begin
  wpkwld : entity work.t
    port map (zfaqyy => jarclfdxke);
  tfhojoh : entity work.ttbgphhbq
    port map (hgttwdmad => n, ktb => lauzwar, olicheysqw => sjhb);
  mbjotdclp : entity work.t
    port map (zfaqyy => jarclfdxke);
  
  -- Single-driven assignments
  gqjcjqwn <= 2#1.11#;
  
  -- Multi-driven assignments
  n <= (others => '0');
  sjhb <= '-';
end yw;

entity fiuyrirnji is
  port (pyzu : buffer bit; xjztc : buffer character; ibou : out time);
end fiuyrirnji;

architecture fb of fiuyrirnji is
  signal mezrtugii : bit_vector(0 downto 4);
  signal nbptuxu : bit_vector(0 downto 4);
  signal yzkrlfnao : real;
  signal ipavmwso : integer;
begin
  wuth : entity work.rsp
    port map (ndsoj => ipavmwso, ppfahypzl => ibou, gqjcjqwn => yzkrlfnao);
  tihm : entity work.t
    port map (zfaqyy => nbptuxu);
  knuodgm : entity work.t
    port map (zfaqyy => mezrtugii);
  
  -- Single-driven assignments
  ibou <= 16#1_6# ps;
  nbptuxu <= (others => '0');
  pyzu <= '1';
  ipavmwso <= ipavmwso;
  xjztc <= 'x';
end fb;



-- Seed after: 867639155030781146,11481034001933599325
