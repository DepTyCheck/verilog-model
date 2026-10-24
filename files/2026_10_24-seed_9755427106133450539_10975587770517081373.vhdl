-- Seed: 9755427106133450539,10975587770517081373

library ieee;
use ieee.std_logic_1164.all;

entity s is
  port (spvldee : in time; kiay : in std_logic_vector(3 downto 2); mcau : out std_logic_vector(2 downto 2); qlydj : linkage integer);
end s;

architecture fwmekt of s is
  
begin
  -- Multi-driven assignments
  mcau <= (others => '0');
  mcau <= "1";
  mcau <= (others => '-');
  mcau <= "L";
end fwmekt;

library ieee;
use ieee.std_logic_1164.all;

entity x is
  port (tgdndmipnp : buffer time; ch : in severity_level; ztzbpd : linkage std_logic_vector(1 to 2); kxas : inout std_logic_vector(2 downto 1));
end x;

library ieee;
use ieee.std_logic_1164.all;

architecture jvsfblk of x is
  signal cb : integer;
  signal covs : std_logic_vector(2 downto 2);
  signal rn : time;
  signal imtl : integer;
  signal jvsiysgrfb : std_logic_vector(2 downto 2);
  signal jvih : integer;
  signal ciqpncek : std_logic_vector(2 downto 2);
  signal kuozqssxzm : std_logic_vector(3 downto 2);
begin
  iamrrdveqb : entity work.s
    port map (spvldee => tgdndmipnp, kiay => kuozqssxzm, mcau => ciqpncek, qlydj => jvih);
  o : entity work.s
    port map (spvldee => tgdndmipnp, kiay => kuozqssxzm, mcau => jvsiysgrfb, qlydj => imtl);
  gndlg : entity work.s
    port map (spvldee => rn, kiay => kxas, mcau => covs, qlydj => cb);
  
  -- Single-driven assignments
  tgdndmipnp <= tgdndmipnp;
  
  -- Multi-driven assignments
  kxas <= kxas;
  ciqpncek <= covs;
end jvsfblk;



-- Seed after: 13641425053099483237,10975587770517081373
