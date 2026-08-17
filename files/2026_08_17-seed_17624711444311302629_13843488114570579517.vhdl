-- Seed: 17624711444311302629,13843488114570579517

library ieee;
use ieee.std_logic_1164.all;

entity pnxmqi is
  port (fjbrx : in time; gpglfjmen : in boolean; zavpovflq : buffer std_logic);
end pnxmqi;

architecture ugudjtv of pnxmqi is
  
begin
  
end ugudjtv;

library ieee;
use ieee.std_logic_1164.all;

entity t is
  port (eqsjhf : out std_logic_vector(1 to 2); te : inout character; rbvy : out std_logic);
end t;

library ieee;
use ieee.std_logic_1164.all;

architecture bunuxch of t is
  signal ynwcdgi : std_logic;
  signal ujh : boolean;
  signal z : time;
  signal nonqwto : boolean;
  signal khi : time;
begin
  gq : entity work.pnxmqi
    port map (fjbrx => khi, gpglfjmen => nonqwto, zavpovflq => rbvy);
  bjbh : entity work.pnxmqi
    port map (fjbrx => z, gpglfjmen => ujh, zavpovflq => ynwcdgi);
end bunuxch;

entity nevurfdpir is
  port (puvg : buffer time; lzdsf : in string(4 downto 3));
end nevurfdpir;

library ieee;
use ieee.std_logic_1164.all;

architecture zuscn of nevurfdpir is
  signal xpvyim : std_logic;
  signal izj : character;
  signal okbehoh : character;
  signal ejmgkutag : std_logic_vector(1 to 2);
  signal qlzjsq : std_logic;
  signal fvzuwf : boolean;
  signal wbw : time;
begin
  fszqxb : entity work.pnxmqi
    port map (fjbrx => wbw, gpglfjmen => fvzuwf, zavpovflq => qlzjsq);
  dp : entity work.t
    port map (eqsjhf => ejmgkutag, te => okbehoh, rbvy => qlzjsq);
  zxlnuf : entity work.t
    port map (eqsjhf => ejmgkutag, te => izj, rbvy => xpvyim);
  
  -- Single-driven assignments
  puvg <= 2#1_0_0_1.1_1# ps;
  
  -- Multi-driven assignments
  qlzjsq <= '0';
end zuscn;



-- Seed after: 1092923485737473449,13843488114570579517
