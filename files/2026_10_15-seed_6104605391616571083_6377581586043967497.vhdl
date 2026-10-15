-- Seed: 6104605391616571083,6377581586043967497

entity uquqj is
  port (mfpezjejt : linkage string(4 to 4); yyfbhzu : out time; vogzf : in time);
end uquqj;

architecture npidmntfjy of uquqj is
  
begin
  -- Single-driven assignments
  yyfbhzu <= 41320.12 us;
end npidmntfjy;

library ieee;
use ieee.std_logic_1164.all;

entity jpjtceaspp is
  port (zyxnrs : out time; e : linkage std_logic);
end jpjtceaspp;

architecture kwvhpayqa of jpjtceaspp is
  signal lgjop : string(4 to 4);
  signal hsmbyzvw : time;
  signal nqwqukpul : string(4 to 4);
  signal qc : time;
  signal uy : string(4 to 4);
begin
  ami : entity work.uquqj
    port map (mfpezjejt => uy, yyfbhzu => zyxnrs, vogzf => qc);
  j : entity work.uquqj
    port map (mfpezjejt => nqwqukpul, yyfbhzu => qc, vogzf => hsmbyzvw);
  wndi : entity work.uquqj
    port map (mfpezjejt => lgjop, yyfbhzu => hsmbyzvw, vogzf => zyxnrs);
end kwvhpayqa;

library ieee;
use ieee.std_logic_1164.all;

entity d is
  port (rtktxgtp : out real; ryad : out std_logic; soqmq : buffer std_logic; jzr : inout string(2 to 4));
end d;

architecture klyum of d is
  signal oltb : string(4 to 4);
  signal cy : time;
  signal da : time;
  signal lgknrqv : time;
  signal vbgdaix : string(4 to 4);
begin
  wrdqufdf : entity work.uquqj
    port map (mfpezjejt => vbgdaix, yyfbhzu => lgknrqv, vogzf => da);
  tdipvqik : entity work.jpjtceaspp
    port map (zyxnrs => cy, e => soqmq);
  fqkliaprq : entity work.uquqj
    port map (mfpezjejt => oltb, yyfbhzu => da, vogzf => cy);
  
  -- Single-driven assignments
  jzr <= "jhj";
end klyum;



-- Seed after: 16853288232392865919,6377581586043967497
