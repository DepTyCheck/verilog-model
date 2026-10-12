-- Seed: 15394555318156839007,3426943171337919827

library ieee;
use ieee.std_logic_1164.all;

entity j is
  port (ukikpiqwt : linkage std_logic_vector(4 to 0));
end j;

architecture bqw of j is
  
begin
  
end bqw;

entity jik is
  port (z : linkage string(2 to 3); baxaofi : out time; pwbaoqgwu : in bit);
end jik;

library ieee;
use ieee.std_logic_1164.all;

architecture mgn of jik is
  signal zqqrhrjm : std_logic_vector(4 to 0);
  signal vu : std_logic_vector(4 to 0);
begin
  ewlviyqb : entity work.j
    port map (ukikpiqwt => vu);
  iql : entity work.j
    port map (ukikpiqwt => zqqrhrjm);
  s : entity work.j
    port map (ukikpiqwt => vu);
  
  -- Single-driven assignments
  baxaofi <= baxaofi;
  
  -- Multi-driven assignments
  vu <= zqqrhrjm;
  vu <= (others => '0');
  vu <= vu;
  zqqrhrjm <= vu;
end mgn;

library ieee;
use ieee.std_logic_1164.all;

entity nir is
  port (imjtnqnt : out std_logic);
end nir;

library ieee;
use ieee.std_logic_1164.all;

architecture dbrvxsac of nir is
  signal vfyhzdwr : std_logic_vector(4 to 0);
  signal ksnykgbyuo : std_logic_vector(4 to 0);
  signal whagbtwp : std_logic_vector(4 to 0);
begin
  pjykifutdt : entity work.j
    port map (ukikpiqwt => whagbtwp);
  gkio : entity work.j
    port map (ukikpiqwt => ksnykgbyuo);
  xjcwphosg : entity work.j
    port map (ukikpiqwt => vfyhzdwr);
  
  -- Multi-driven assignments
  imjtnqnt <= 'L';
  ksnykgbyuo <= "";
  imjtnqnt <= 'Z';
end dbrvxsac;

entity jwoh is
  port (svsj : out real; tvx : buffer time; yauhk : linkage boolean_vector(3 to 4); fruzslgz : in real);
end jwoh;

library ieee;
use ieee.std_logic_1164.all;

architecture mfqobfyc of jwoh is
  signal hgsij : std_logic_vector(4 to 0);
  signal jud : std_logic_vector(4 to 0);
  signal hadh : std_logic;
  signal ptmd : bit;
  signal tgzjvz : time;
  signal fwbr : string(2 to 3);
begin
  ri : entity work.jik
    port map (z => fwbr, baxaofi => tgzjvz, pwbaoqgwu => ptmd);
  uihokcd : entity work.nir
    port map (imjtnqnt => hadh);
  rhhkc : entity work.j
    port map (ukikpiqwt => jud);
  fjrmuk : entity work.j
    port map (ukikpiqwt => hgsij);
  
  -- Single-driven assignments
  svsj <= 16#B4.2_F_E#;
  
  -- Multi-driven assignments
  hgsij <= jud;
end mfqobfyc;



-- Seed after: 10501176369279031041,3426943171337919827
