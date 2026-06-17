-- Seed: 10490221224714433836,10557070023141912087

library ieee;
use ieee.std_logic_1164.all;

entity cpyf is
  port (shtybf : inout std_logic_vector(0 downto 4); biqjyi : out time; un : in time);
end cpyf;

architecture dve of cpyf is
  
begin
  -- Multi-driven assignments
  shtybf <= "";
end dve;

library ieee;
use ieee.std_logic_1164.all;

entity rkgw is
  port (fc : linkage bit; i : inout std_logic; ejho : out real);
end rkgw;

library ieee;
use ieee.std_logic_1164.all;

architecture hidokqrzc of rkgw is
  signal xqwqam : time;
  signal vgsfzjth : std_logic_vector(0 downto 4);
  signal yjmobwg : time;
  signal rfcy : time;
  signal oqesxkqgsg : time;
  signal vpmwo : std_logic_vector(0 downto 4);
  signal iivdl : time;
  signal y : std_logic_vector(0 downto 4);
begin
  dtjtrw : entity work.cpyf
    port map (shtybf => y, biqjyi => iivdl, un => iivdl);
  rjgkgj : entity work.cpyf
    port map (shtybf => vpmwo, biqjyi => oqesxkqgsg, un => rfcy);
  cd : entity work.cpyf
    port map (shtybf => y, biqjyi => rfcy, un => yjmobwg);
  aypr : entity work.cpyf
    port map (shtybf => vgsfzjth, biqjyi => yjmobwg, un => xqwqam);
  
  -- Single-driven assignments
  ejho <= 0413.024;
  xqwqam <= 1 ns;
  
  -- Multi-driven assignments
  y <= (others => '0');
  i <= 'X';
  i <= 'U';
  y <= "";
end hidokqrzc;

entity yb is
  port (ocmqju : linkage real_vector(4 to 3); elur : in character);
end yb;

library ieee;
use ieee.std_logic_1164.all;

architecture w of yb is
  signal wrmzqapt : std_logic_vector(0 downto 4);
  signal gxy : time;
  signal oddgbhp : std_logic_vector(0 downto 4);
  signal uf : time;
  signal dmcal : time;
  signal jlsmrm : std_logic_vector(0 downto 4);
begin
  tajnhq : entity work.cpyf
    port map (shtybf => jlsmrm, biqjyi => dmcal, un => uf);
  hzv : entity work.cpyf
    port map (shtybf => oddgbhp, biqjyi => gxy, un => uf);
  gqjws : entity work.cpyf
    port map (shtybf => wrmzqapt, biqjyi => uf, un => gxy);
  
  -- Multi-driven assignments
  jlsmrm <= "";
  jlsmrm <= "";
end w;

entity bvsmv is
  port (qyfwcb : buffer integer_vector(4 downto 4); i : buffer severity_level; jjbhzkza : out integer);
end bvsmv;

architecture vpgz of bvsmv is
  signal vydetyhle : character;
  signal y : real_vector(4 to 3);
begin
  socjdwl : entity work.yb
    port map (ocmqju => y, elur => vydetyhle);
end vpgz;



-- Seed after: 1744677354822682459,10557070023141912087
