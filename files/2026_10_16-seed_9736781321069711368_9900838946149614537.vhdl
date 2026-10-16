-- Seed: 9736781321069711368,9900838946149614537

library ieee;
use ieee.std_logic_1164.all;

entity dkvozingex is
  port ( iawcn : in time_vector(0 to 3)
  ; mjxvqaomau : in std_logic_vector(3 to 3)
  ; jslweffowd : in std_logic_vector(2 downto 2)
  ; nojumxkk : in integer_vector(3 downto 2)
  );
end dkvozingex;

architecture qloafmzgl of dkvozingex is
  
begin
  
end qloafmzgl;

entity uqowjseao is
  port (mkvklfnw : buffer severity_level);
end uqowjseao;

library ieee;
use ieee.std_logic_1164.all;

architecture abmze of uqowjseao is
  signal zutvnwvgi : integer_vector(3 downto 2);
  signal bpmezgwa : std_logic_vector(2 downto 2);
  signal ftywjm : integer_vector(3 downto 2);
  signal dfz : integer_vector(3 downto 2);
  signal pdv : integer_vector(3 downto 2);
  signal uqvhcylxs : std_logic_vector(3 to 3);
  signal bwyfhtmx : time_vector(0 to 3);
begin
  qtgytkjab : entity work.dkvozingex
    port map (iawcn => bwyfhtmx, mjxvqaomau => uqvhcylxs, jslweffowd => uqvhcylxs, nojumxkk => pdv);
  mtvqpankk : entity work.dkvozingex
    port map (iawcn => bwyfhtmx, mjxvqaomau => uqvhcylxs, jslweffowd => uqvhcylxs, nojumxkk => dfz);
  fzuzim : entity work.dkvozingex
    port map (iawcn => bwyfhtmx, mjxvqaomau => uqvhcylxs, jslweffowd => uqvhcylxs, nojumxkk => ftywjm);
  lurda : entity work.dkvozingex
    port map (iawcn => bwyfhtmx, mjxvqaomau => uqvhcylxs, jslweffowd => bpmezgwa, nojumxkk => zutvnwvgi);
  
  -- Single-driven assignments
  dfz <= pdv;
  pdv <= pdv;
  bwyfhtmx <= bwyfhtmx;
  mkvklfnw <= ERROR;
  
  -- Multi-driven assignments
  uqvhcylxs <= (others => 'Z');
  uqvhcylxs <= "L";
end abmze;

entity ea is
  port (gxie : in severity_level; vtlrlwqg : linkage time);
end ea;

library ieee;
use ieee.std_logic_1164.all;

architecture pfk of ea is
  signal rrdruvtoxv : std_logic_vector(2 downto 2);
  signal wuzyzodt : time_vector(0 to 3);
  signal yvfs : integer_vector(3 downto 2);
  signal revv : std_logic_vector(3 to 3);
  signal xzcknexqu : time_vector(0 to 3);
begin
  havvn : entity work.dkvozingex
    port map (iawcn => xzcknexqu, mjxvqaomau => revv, jslweffowd => revv, nojumxkk => yvfs);
  hqbzpi : entity work.dkvozingex
    port map (iawcn => wuzyzodt, mjxvqaomau => revv, jslweffowd => rrdruvtoxv, nojumxkk => yvfs);
  
  -- Single-driven assignments
  xzcknexqu <= (8#1_4_4# ms, 8#5.2_4_1_6_1# ps, 2_4 fs, 2#100# us);
  wuzyzodt <= (0 hr, 16#C_7_A_D# ps, 2 ms, 8#1_3_5_5# ps);
  yvfs <= yvfs;
  
  -- Multi-driven assignments
  revv <= revv;
  revv <= (others => 'X');
  revv <= (others => '1');
  revv <= revv;
end pfk;

library ieee;
use ieee.std_logic_1164.all;

entity mkwazad is
  port (telsa : linkage time; rwk : linkage real; kxbtmr : in real; dwxgjtjc : in std_logic);
end mkwazad;

architecture waqrbmlmkx of mkwazad is
  
begin
  
end waqrbmlmkx;



-- Seed after: 7499844759909784780,9900838946149614537
