-- Seed: 8835215492255401564,4404421571376382767

library ieee;
use ieee.std_logic_1164.all;

entity pqdtmkfqc is
  port (rvxx : buffer std_logic_vector(3 downto 1); zhbaqp : buffer time_vector(4 to 3));
end pqdtmkfqc;

architecture jrmohi of pqdtmkfqc is
  
begin
  -- Single-driven assignments
  zhbaqp <= (others => 0 ns);
  
  -- Multi-driven assignments
  rvxx <= rvxx;
  rvxx <= ('W', 'W', '-');
  rvxx <= ('W', 'Z', 'Z');
end jrmohi;

entity bkd is
  port (odccxb : in real);
end bkd;

library ieee;
use ieee.std_logic_1164.all;

architecture zzreyob of bkd is
  signal avaiyveh : time_vector(4 to 3);
  signal yca : std_logic_vector(3 downto 1);
  signal jemjx : time_vector(4 to 3);
  signal mxmswkkis : std_logic_vector(3 downto 1);
begin
  hf : entity work.pqdtmkfqc
    port map (rvxx => mxmswkkis, zhbaqp => jemjx);
  zrvnae : entity work.pqdtmkfqc
    port map (rvxx => yca, zhbaqp => avaiyveh);
end zzreyob;

library ieee;
use ieee.std_logic_1164.all;

entity iakjpx is
  port (ejieaqb : linkage std_logic_vector(3 downto 0); hderswyvvv : inout time; jbnq : buffer std_logic_vector(4 downto 4); ascik : linkage std_logic);
end iakjpx;

architecture lxudql of iakjpx is
  
begin
  -- Single-driven assignments
  hderswyvvv <= 4.0_0 ns;
end lxudql;

entity qwat is
  port (vusemfxd : inout time; cziydza : out time);
end qwat;

library ieee;
use ieee.std_logic_1164.all;

architecture dmewdtthg of qwat is
  signal rhee : time_vector(4 to 3);
  signal etxyobiggr : std_logic_vector(3 downto 1);
begin
  w : entity work.pqdtmkfqc
    port map (rvxx => etxyobiggr, zhbaqp => rhee);
end dmewdtthg;



-- Seed after: 13875385578181126083,4404421571376382767
