-- Seed: 11382169478394897441,17047277710231705797

library ieee;
use ieee.std_logic_1164.all;

entity ttcicfrwk is
  port (baxknib : inout real_vector(2 to 1); td : linkage bit_vector(2 to 4); wocafb : out std_logic_vector(4 to 0); ztcj : linkage integer);
end ttcicfrwk;

architecture xyj of ttcicfrwk is
  
begin
  -- Single-driven assignments
  baxknib <= (others => 0.0);
end xyj;

library ieee;
use ieee.std_logic_1164.all;

entity uwr is
  port (fo : linkage std_logic_vector(0 downto 2); thphexdev : buffer real; hajbze : buffer real);
end uwr;

library ieee;
use ieee.std_logic_1164.all;

architecture zabmldnoa of uwr is
  signal ligag : integer;
  signal mlclulnhd : std_logic_vector(4 to 0);
  signal p : bit_vector(2 to 4);
  signal wrvchxtfey : real_vector(2 to 1);
begin
  pgvzlgkzt : entity work.ttcicfrwk
    port map (baxknib => wrvchxtfey, td => p, wocafb => mlclulnhd, ztcj => ligag);
  
  -- Single-driven assignments
  hajbze <= 0_0_2_2_1.4410;
  thphexdev <= 2#1.1_1_0#;
  
  -- Multi-driven assignments
  mlclulnhd <= (others => '0');
  mlclulnhd <= "";
  mlclulnhd <= (others => '0');
end zabmldnoa;

library ieee;
use ieee.std_logic_1164.all;

entity efqtswujjj is
  port ( udhzy : buffer real_vector(4 downto 4)
  ; o : in std_logic_vector(3 to 3)
  ; uobuax : buffer std_logic_vector(3 to 0)
  ; iqmhgn : out std_logic_vector(3 to 3)
  );
end efqtswujjj;

architecture ekapdsxhf of efqtswujjj is
  signal xzpyxepc : real;
  signal ifscal : real;
  signal cv : integer;
  signal n : bit_vector(2 to 4);
  signal aaiwfif : real_vector(2 to 1);
  signal m : real;
  signal qrtzo : real;
  signal tuybjtbu : integer;
  signal lgcoxld : bit_vector(2 to 4);
  signal bvrrx : real_vector(2 to 1);
begin
  nikgnqlrpc : entity work.ttcicfrwk
    port map (baxknib => bvrrx, td => lgcoxld, wocafb => uobuax, ztcj => tuybjtbu);
  ad : entity work.uwr
    port map (fo => uobuax, thphexdev => qrtzo, hajbze => m);
  tasctyjqpn : entity work.ttcicfrwk
    port map (baxknib => aaiwfif, td => n, wocafb => uobuax, ztcj => cv);
  vabg : entity work.uwr
    port map (fo => uobuax, thphexdev => ifscal, hajbze => xzpyxepc);
  
  -- Single-driven assignments
  udhzy <= (others => 8#1407.44475#);
  
  -- Multi-driven assignments
  iqmhgn <= (others => '1');
  uobuax <= "";
  iqmhgn <= (others => 'Z');
end ekapdsxhf;



-- Seed after: 13864953471764128166,17047277710231705797
