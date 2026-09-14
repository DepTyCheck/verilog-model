-- Seed: 17413358532652051003,13196211255131729027

library ieee;
use ieee.std_logic_1164.all;

entity rxbbi is
  port (hih : linkage std_logic; xazluryd : out real; j : out time; kybxdvxn : out time);
end rxbbi;

architecture lqmyff of rxbbi is
  
begin
  -- Single-driven assignments
  kybxdvxn <= j;
  xazluryd <= xazluryd;
  j <= kybxdvxn;
end lqmyff;

library ieee;
use ieee.std_logic_1164.all;

entity skgmnrpw is
  port (mzvqvstvm : buffer std_logic_vector(3 to 2); fhd : buffer integer_vector(1 to 0));
end skgmnrpw;

library ieee;
use ieee.std_logic_1164.all;

architecture uy of skgmnrpw is
  signal iof : time;
  signal ovnauqb : time;
  signal h : real;
  signal unwgtgyh : std_logic;
  signal uxg : time;
  signal sqwvsivc : time;
  signal ygytkvbrb : real;
  signal ykat : std_logic;
  signal dzjytjphb : time;
  signal gqqcirqka : time;
  signal gi : real;
  signal qvkxjp : time;
  signal acw : time;
  signal epfddmi : real;
  signal nroenokzi : std_logic;
begin
  oglu : entity work.rxbbi
    port map (hih => nroenokzi, xazluryd => epfddmi, j => acw, kybxdvxn => qvkxjp);
  mgd : entity work.rxbbi
    port map (hih => nroenokzi, xazluryd => gi, j => gqqcirqka, kybxdvxn => dzjytjphb);
  njaz : entity work.rxbbi
    port map (hih => ykat, xazluryd => ygytkvbrb, j => sqwvsivc, kybxdvxn => uxg);
  ru : entity work.rxbbi
    port map (hih => unwgtgyh, xazluryd => h, j => ovnauqb, kybxdvxn => iof);
  
  -- Single-driven assignments
  fhd <= (others => 0);
  
  -- Multi-driven assignments
  mzvqvstvm <= (others => '0');
  ykat <= nroenokzi;
  unwgtgyh <= 'Z';
end uy;



-- Seed after: 13549427671961934133,13196211255131729027
