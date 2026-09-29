-- Seed: 16176014959868604461,10940991575366938685

entity pgynizcbor is
  port (vkrd : out time; pmunhfqee : in integer_vector(0 to 3); shcaaqukk : inout integer);
end pgynizcbor;

architecture wywv of pgynizcbor is
  
begin
  
end wywv;

entity cqcdakon is
  port (jeowkllx : out time; ta : buffer time_vector(2 to 3); rr : in boolean);
end cqcdakon;

architecture deiwtd of cqcdakon is
  signal kbctdsjosc : integer;
  signal vncgwr : time;
  signal zjqzoe : integer;
  signal ckha : integer;
  signal li : integer_vector(0 to 3);
  signal ze : time;
  signal uusxvv : integer;
  signal mspqxvef : integer_vector(0 to 3);
  signal hbqrewpiem : time;
begin
  udibwmcjv : entity work.pgynizcbor
    port map (vkrd => hbqrewpiem, pmunhfqee => mspqxvef, shcaaqukk => uusxvv);
  zcjnx : entity work.pgynizcbor
    port map (vkrd => ze, pmunhfqee => li, shcaaqukk => ckha);
  fquwpoj : entity work.pgynizcbor
    port map (vkrd => jeowkllx, pmunhfqee => mspqxvef, shcaaqukk => zjqzoe);
  ngguvqd : entity work.pgynizcbor
    port map (vkrd => vncgwr, pmunhfqee => mspqxvef, shcaaqukk => kbctdsjosc);
  
  -- Single-driven assignments
  ta <= ta;
  li <= li;
  mspqxvef <= (16#7#, 03, 16#8DA#, 1);
end deiwtd;



-- Seed after: 17545969898147689205,10940991575366938685
