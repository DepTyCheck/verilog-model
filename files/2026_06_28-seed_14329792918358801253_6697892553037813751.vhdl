-- Seed: 14329792918358801253,6697892553037813751

library ieee;
use ieee.std_logic_1164.all;

entity u is
  port (cudgkl : inout severity_level; mbxjiamv : buffer std_logic_vector(0 downto 3); ewth : inout std_logic; rlwnw : buffer std_logic_vector(3 to 0));
end u;

architecture usbztmim of u is
  
begin
  -- Single-driven assignments
  cudgkl <= NOTE;
  
  -- Multi-driven assignments
  rlwnw <= "";
  rlwnw <= (others => '0');
  rlwnw <= (others => '0');
end usbztmim;

entity rqzkcwyh is
  port (o : in integer; ydgra : in real);
end rqzkcwyh;

library ieee;
use ieee.std_logic_1164.all;

architecture sxkrs of rqzkcwyh is
  signal swzzezos : std_logic_vector(3 to 0);
  signal ixmdyb : std_logic;
  signal ikpmblq : severity_level;
  signal puoutjrdta : std_logic;
  signal dutnd : std_logic_vector(0 downto 3);
  signal mtamjokonl : severity_level;
  signal wqveq : std_logic_vector(0 downto 3);
  signal xsvaim : severity_level;
  signal ugtovjmjnb : std_logic;
  signal mmjmahzdq : std_logic_vector(3 to 0);
  signal bafaqxym : severity_level;
begin
  nz : entity work.u
    port map (cudgkl => bafaqxym, mbxjiamv => mmjmahzdq, ewth => ugtovjmjnb, rlwnw => mmjmahzdq);
  kcpr : entity work.u
    port map (cudgkl => xsvaim, mbxjiamv => wqveq, ewth => ugtovjmjnb, rlwnw => mmjmahzdq);
  ldkgpyermw : entity work.u
    port map (cudgkl => mtamjokonl, mbxjiamv => dutnd, ewth => puoutjrdta, rlwnw => wqveq);
  ixv : entity work.u
    port map (cudgkl => ikpmblq, mbxjiamv => wqveq, ewth => ixmdyb, rlwnw => swzzezos);
end sxkrs;

library ieee;
use ieee.std_logic_1164.all;

entity hmrz is
  port (n : out std_logic; gssrhetvyb : inout bit; maocxm : buffer real);
end hmrz;

library ieee;
use ieee.std_logic_1164.all;

architecture dqrr of hmrz is
  signal f : std_logic_vector(3 to 0);
  signal wj : std_logic;
  signal vaeytoqy : std_logic_vector(0 downto 3);
  signal zwewgrljt : severity_level;
  signal brghor : real;
  signal ignuryq : integer;
begin
  nbudyj : entity work.rqzkcwyh
    port map (o => ignuryq, ydgra => brghor);
  icmbxzx : entity work.u
    port map (cudgkl => zwewgrljt, mbxjiamv => vaeytoqy, ewth => wj, rlwnw => f);
  
  -- Single-driven assignments
  gssrhetvyb <= '0';
  ignuryq <= 16#9#;
  maocxm <= 16#9_3_4.9#;
  brghor <= 2#10.0_0_0#;
  
  -- Multi-driven assignments
  n <= '1';
end dqrr;



-- Seed after: 3358881341760285024,6697892553037813751
