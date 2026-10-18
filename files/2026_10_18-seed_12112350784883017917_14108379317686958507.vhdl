-- Seed: 12112350784883017917,14108379317686958507

library ieee;
use ieee.std_logic_1164.all;

entity vxwte is
  port (swd : out std_logic_vector(2 to 4));
end vxwte;

architecture yarj of vxwte is
  
begin
  
end yarj;

entity vfeob is
  port (dizjqfi : in bit_vector(1 to 4); eun : out real);
end vfeob;

library ieee;
use ieee.std_logic_1164.all;

architecture bbdimiyymy of vfeob is
  signal gbdyh : std_logic_vector(2 to 4);
  signal lr : std_logic_vector(2 to 4);
  signal riqirv : std_logic_vector(2 to 4);
begin
  pmdjpbszwk : entity work.vxwte
    port map (swd => riqirv);
  a : entity work.vxwte
    port map (swd => riqirv);
  olpqnnmtc : entity work.vxwte
    port map (swd => lr);
  hdglqrcxx : entity work.vxwte
    port map (swd => gbdyh);
  
  -- Single-driven assignments
  eun <= eun;
  
  -- Multi-driven assignments
  lr <= lr;
end bbdimiyymy;

entity altddxbpwn is
  port (q : buffer time);
end altddxbpwn;

library ieee;
use ieee.std_logic_1164.all;

architecture j of altddxbpwn is
  signal naxrmawtsb : std_logic_vector(2 to 4);
begin
  jms : entity work.vxwte
    port map (swd => naxrmawtsb);
  
  -- Single-driven assignments
  q <= 16#C.8_B_8_4_4# ms;
  
  -- Multi-driven assignments
  naxrmawtsb <= naxrmawtsb;
  naxrmawtsb <= naxrmawtsb;
  naxrmawtsb <= ('-', 'X', '-');
end j;



-- Seed after: 17700233461827001182,14108379317686958507
