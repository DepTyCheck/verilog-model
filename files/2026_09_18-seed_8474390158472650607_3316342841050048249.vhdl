-- Seed: 8474390158472650607,3316342841050048249

entity lqriqmqv is
  port (awzrxuh : buffer time; nu : inout time; yimxhqqi : inout real; m : linkage integer);
end lqriqmqv;

architecture zrsebe of lqriqmqv is
  
begin
  -- Single-driven assignments
  awzrxuh <= nu;
  nu <= 4432 ns;
  yimxhqqi <= 16#1.3#;
end zrsebe;

entity mmrqedunms is
  port (otpckgdqd : buffer real; fbjstt : linkage integer; zokoh : linkage real);
end mmrqedunms;

architecture hvtiwho of mmrqedunms is
  signal a : real;
  signal jvrc : time;
  signal rxjgtinsks : time;
  signal lqoeprnxei : integer;
  signal wkqpfsuplc : time;
  signal pqdaj : time;
begin
  kc : entity work.lqriqmqv
    port map (awzrxuh => pqdaj, nu => wkqpfsuplc, yimxhqqi => otpckgdqd, m => lqoeprnxei);
  tskvo : entity work.lqriqmqv
    port map (awzrxuh => rxjgtinsks, nu => jvrc, yimxhqqi => a, m => fbjstt);
end hvtiwho;



-- Seed after: 15885498098284041734,3316342841050048249
