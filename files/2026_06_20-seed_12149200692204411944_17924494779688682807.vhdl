-- Seed: 12149200692204411944,17924494779688682807

entity dbtd is
  port (orgsyd : out real_vector(0 to 4); wd : in real);
end dbtd;

architecture jgimhmgt of dbtd is
  
begin
  -- Single-driven assignments
  orgsyd <= (1.3303, 4.1, 0030.2, 4000.3411, 3_2_0_1_3.04);
end jgimhmgt;

entity copevuanjy is
  port (yqv : in integer);
end copevuanjy;

architecture blhlhui of copevuanjy is
  signal xuwlzukckw : real;
  signal c : real_vector(0 to 4);
  signal pmrr : real_vector(0 to 4);
  signal plom : real;
  signal p : real_vector(0 to 4);
  signal vqslqbhlk : real;
  signal zaluhfhe : real_vector(0 to 4);
begin
  cxwtr : entity work.dbtd
    port map (orgsyd => zaluhfhe, wd => vqslqbhlk);
  ofjjtsenp : entity work.dbtd
    port map (orgsyd => p, wd => plom);
  skzzplx : entity work.dbtd
    port map (orgsyd => pmrr, wd => vqslqbhlk);
  feskpoh : entity work.dbtd
    port map (orgsyd => c, wd => xuwlzukckw);
  
  -- Single-driven assignments
  vqslqbhlk <= 4_0_2_1.2_2_1_4_0;
  plom <= 0_1.14;
  xuwlzukckw <= 2442.34403;
end blhlhui;

library ieee;
use ieee.std_logic_1164.all;

entity bqujnvfenl is
  port (lvnyso : linkage std_logic_vector(4 downto 0); vxlcvyi : in real; phdhgd : linkage time; blqyzdbgwr : in std_logic_vector(1 downto 3));
end bqujnvfenl;

architecture wd of bqujnvfenl is
  
begin
  
end wd;

library ieee;
use ieee.std_logic_1164.all;

entity s is
  port (ri : in time; tncjg : inout std_logic_vector(3 to 3); c : buffer bit; wxtmlxd : inout std_logic_vector(4 to 2));
end s;

library ieee;
use ieee.std_logic_1164.all;

architecture wjibpuol of s is
  signal vmzwpcdssx : std_logic_vector(1 downto 3);
  signal cv : time;
  signal dzl : real;
  signal yh : std_logic_vector(4 downto 0);
  signal yrl : real;
  signal wd : real_vector(0 to 4);
  signal saxkksuzhh : integer;
begin
  ltvm : entity work.copevuanjy
    port map (yqv => saxkksuzhh);
  dqe : entity work.dbtd
    port map (orgsyd => wd, wd => yrl);
  krmhvyxxl : entity work.bqujnvfenl
    port map (lvnyso => yh, vxlcvyi => dzl, phdhgd => cv, blqyzdbgwr => vmzwpcdssx);
  
  -- Multi-driven assignments
  wxtmlxd <= (others => '0');
  wxtmlxd <= "";
  tncjg <= (others => 'H');
end wjibpuol;



-- Seed after: 16855881220507284883,17924494779688682807
